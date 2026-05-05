.llm_chat_cache <- new.env(parent = emptyenv())

.llm_cache_ensure_dir <- function(path) {
    cache_dir <- dirname(path)
    if (!dir.exists(cache_dir)) {
        dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    }

    path
}

.llm_cache_default_path <- function(path = NULL) {
    if (is.null(path)) {
        return(.llm_cache_ensure_dir(file.path(getwd(), ".cache_agrillm/cache.rds")))
    }

    stopifnot(is.character(path), length(path) == 1, nzchar(path))
    .llm_cache_ensure_dir(path)
}

.llm_cache_key <- function(prompt, provider, provider_config, dots) {
    stopifnot(is.character(prompt), length(prompt) == 1)
    stopifnot(is.character(provider), length(provider) == 1)

    request_payload <- list(
        prompt = prompt,
        provider = provider,
        provider_config = provider_config,
        dots = dots
    )

    paste(
        provider,
        digest::digest(request_payload, algo = "xxhash64", serialize = TRUE),
        sep = ":"
    )
}

.llm_cache_get <- function(key) {
    if (!exists(key, envir = .llm_chat_cache, inherits = FALSE)) {
        return(NULL)
    }

    get(key, envir = .llm_chat_cache, inherits = FALSE)$response
}

.llm_cache_read_disk <- function(path = NULL) {
    cache_path <- .llm_cache_default_path(path)
    if (!file.exists(cache_path)) {
        return(list())
    }

    payload <- readRDS(cache_path)
    if (!is.list(payload) || is.null(payload$entries) || !is.list(payload$entries)) {
        stop("Invalid LLM cache file: ", cache_path)
    }

    payload$entries
}

.llm_cache_get_disk <- function(key, path = NULL) {
    disk_entries <- .llm_cache_read_disk(path)
    if (!key %in% names(disk_entries)) {
        return(NULL)
    }

    entry <- disk_entries[[key]]
    assign(key, entry, envir = .llm_chat_cache)
    entry$response
}

.llm_cache_set <- function(key, provider, message, response) {
    assign(
        key,
        list(
            provider = provider,
            message = message,
            response = response,
            created_at = Sys.time()
        ),
        envir = .llm_chat_cache
    )

    invisible(response)
}

.llm_cache_entries <- function(keys = NULL) {
    if (is.null(keys)) {
        keys <- ls(envir = .llm_chat_cache, all.names = TRUE)
    }
    if (length(keys) == 0) {
        return(list())
    }

    mget(keys, envir = .llm_chat_cache, inherits = FALSE)
}

.llm_cache_write_disk <- function(path = NULL, keys = NULL) {
    cache_path <- .llm_cache_default_path(path)

    payload <- list(
        version = 1L,
        saved_at = Sys.time(),
        entries = .llm_cache_entries(keys)
    )

    saveRDS(payload, cache_path)
    invisible(cache_path)
}

.llm_cache_load_disk <- function(path = NULL, replace = FALSE) {
    disk_entries <- .llm_cache_read_disk(path)
    if (replace) {
        llm_cache_clear()
    }
    if (length(disk_entries) == 0) {
        return(invisible(0L))
    }

    list2env(disk_entries, envir = .llm_chat_cache)
    invisible(length(disk_entries))
}

.llm_cache_filter_keys <- function(message = NULL, provider = NULL) {
    cache_keys <- ls(envir = .llm_chat_cache, all.names = TRUE)
    if (length(cache_keys) == 0) {
        return(character())
    }

    Filter(function(key) {
        entry <- get(key, envir = .llm_chat_cache, inherits = FALSE)
        matches_message <- is.null(message) || identical(entry$message, message)
        matches_provider <- is.null(provider) || identical(entry$provider, provider)
        matches_message && matches_provider
    }, cache_keys)
}

.llm_cached_call <- function(
    prompt,
    provider,
    provider_config,
    use_cache,
    persist_cache,
    cache_path,
    dots,
    caller) {
    stopifnot(is.function(caller))

    if (!use_cache) {
        return(caller())
    }

    cache_key <- .llm_cache_key(
        prompt = prompt,
        provider = provider,
        provider_config = provider_config,
        dots = dots
    )
    cached_response <- .llm_cache_get(cache_key)
    if (is.null(cached_response) && persist_cache) {
        cached_response <- .llm_cache_get_disk(cache_key, path = cache_path)
    }
    if (!is.null(cached_response)) {
        return(cached_response)
    }

    response <- caller()
    .llm_cache_set(cache_key, provider = provider, message = prompt, response = response)
    if (persist_cache) {
        .llm_cache_write_disk(path = cache_path)
    }
    response
}

#' Get the default LLM cache file path
#'
#' @param path Optional override path. If omitted, the project-local default
#' cache file path is returned. The parent cache directory is created when it
#' does not already exist.
#' @return Character string path to the cache file.
#' @export
llm_cache_path <- function(path = NULL) {
    .llm_cache_default_path(path)
}

#' Clear LLM chat cache
#'
#' Clears cached LLM responses. You can optionally restrict removal to a single
#' prompt and/or provider.
#'
#' @param message Optional character string. If provided, only matching cache
#' entries are removed.
#' @param provider Optional character string. If provided, only cache entries
#' for that backend are removed.
#' @return Invisibly returns the number of removed cache entries.
#' @export
llm_cache_clear <- function(message = NULL, provider = NULL) {
    if (!is.null(message)) {
        stopifnot(is.character(message), length(message) == 1)
    }
    if (!is.null(provider)) {
        stopifnot(is.character(provider), length(provider) == 1)
    }

    cache_keys <- .llm_cache_filter_keys(message = message, provider = provider)
    if (length(cache_keys) > 0) {
        rm(list = cache_keys, envir = .llm_chat_cache)
    }

    invisible(length(cache_keys))
}

#' Save the in-memory LLM cache to disk
#'
#' @param path Optional cache file path. Default: `.cache_agrillm/cache.rds` in
#' the current working directory.
#' @return Invisibly returns the cache file path written.
#' @export
llm_cache_save <- function(path = NULL) {
    .llm_cache_write_disk(path = path)
}

#' Load an LLM cache file into memory
#'
#' @param path Optional cache file path. Default: `.cache_agrillm/cache.rds` in
#' the current working directory.
#' @param replace Logical. If `TRUE`, clear the current in-memory cache before
#' loading the file. Default: `FALSE`.
#' @return Invisibly returns the number of loaded cache entries.
#' @export
llm_cache_load <- function(path = NULL, replace = FALSE) {
    stopifnot(is.logical(replace), length(replace) == 1, !is.na(replace))
    .llm_cache_load_disk(path = path, replace = replace)
}

#' Delete an LLM cache file from disk
#'
#' @param path Optional cache file path. Default: `.cache_agrillm/cache.rds` in
#' the current working directory.
#' @return Invisibly returns `TRUE` when a file was deleted and `FALSE`
#' otherwise.
#' @export
llm_cache_delete <- function(path = NULL) {
    cache_path <- .llm_cache_default_path(path)
    if (!file.exists(cache_path)) {
        return(invisible(FALSE))
    }

    file.remove(cache_path)
}

#' Inspect LLM chat cache
#'
#' Returns lightweight metadata about cached LLM responses.
#'
#' @param provider Optional character string. If provided, only cache entries
#' for that backend are returned.
#' @return A data frame with cache keys, providers, original messages, and
#' creation times.
#' @export
llm_cache_info <- function(provider = NULL) {
    if (!is.null(provider)) {
        stopifnot(is.character(provider), length(provider) == 1)
    }

    cache_keys <- .llm_cache_filter_keys(provider = provider)
    if (length(cache_keys) == 0) {
        return(data.frame(
            key = character(),
            provider = character(),
            message = character(),
            created_at = as.POSIXct(character()),
            stringsAsFactors = FALSE
        ))
    }

    entries <- mget(cache_keys, envir = .llm_chat_cache, inherits = FALSE)
    created_at <- do.call(c, lapply(entries, function(entry) entry$created_at))

    data.frame(
        key = cache_keys,
        provider = vapply(entries, function(entry) entry$provider, character(1)),
        message = vapply(entries, function(entry) entry$message, character(1)),
        created_at = as.POSIXct(created_at, origin = "1970-01-01", tz = "UTC"),
        stringsAsFactors = FALSE
    )
}

#' Get LLM chat cache size
#'
#' @param provider Optional character string. If provided, only cache entries
#' for that backend are counted.
#' @return Integer count of currently cached LLM responses.
#' @export
llm_cache_size <- function(provider = NULL) {
    if (!is.null(provider)) {
        stopifnot(is.character(provider), length(provider) == 1)
    }

    length(.llm_cache_filter_keys(provider = provider))
}