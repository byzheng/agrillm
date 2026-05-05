.azure_chat <- function(message, deployment, api_version, ...) {
    msg <- tidyllm::llm_message(message)
    tidyllm::azure_openai_chat(
        msg,
        .deployment = deployment,
        .api_version = api_version,
        ...
    )
}

#' High-level function to call Azure OpenAI chat using tidyllm
#'
#' @param message A character string with the prompt or message.
#' @param use_cache Logical. If `TRUE`, reuse a cached response for identical
#' requests within the current R session. Default: `FALSE`.
#' @param persist_cache Logical. If `TRUE`, persist cached responses to disk so
#' they can be reloaded in later sessions. Default: `TRUE`.
#' @param cache_path Optional character string path to the cache file used when
#' `persist_cache = TRUE`. Default: `.cache_agrillm.rds` in the current working
#' directory.
#' @param ... Additional arguments passed to azure_openai_chat.
#' @return The response from Azure OpenAI chat.
#' @export
#' @examples
#' \dontrun{
#' msg <- llm_message("Test prompt")
#' response <- azure_chat(msg)
#' print(response)
#' }
azure_chat <- function(message, use_cache = FALSE, persist_cache = TRUE, cache_path = NULL, ...) {
    stopifnot(is.character(message), length(message) == 1)
    stopifnot(is.logical(use_cache), length(use_cache) == 1, !is.na(use_cache))
    stopifnot(is.logical(persist_cache), length(persist_cache) == 1, !is.na(persist_cache))
    if (!is.null(cache_path)) {
        stopifnot(is.character(cache_path), length(cache_path) == 1, nzchar(cache_path))
    }
    llm_opts <- agrillm_options$get("llm")
    if (llm_opts$provider != "azure") {
        stop("Only Azure provider is supported in this function.")
    }
    # Extract Azure config
    deployment <- llm_opts$azure$deployment_name
    api_version <- llm_opts$azure$api_version
    dots <- list(...)

    .llm_cached_call(
        prompt = message,
        provider = llm_opts$provider,
        provider_config = llm_opts$azure,
        use_cache = use_cache,
        persist_cache = persist_cache,
        cache_path = cache_path,
        dots = dots,
        caller = function() {
            do.call(
                .azure_chat,
                c(
                    list(
                        message = message,
                        deployment = deployment,
                        api_version = api_version
                    ),
                    dots
                )
            )
        }
    )
}
