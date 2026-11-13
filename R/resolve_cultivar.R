#' Resolve cultivar from free-text input
#' 
#' @details 
#' 
#' This function attempts to interpret a farmer's free-text description of a cultivar using an LLM. It returns a structured representation with the best matching cultivar. If the input cannot be resolved, it returns "unknown". 
#' 
#' The function first checks for direct matches against a list of supported cultivars for the specified crop. If no deterministic match is found and AI fallback is enabled, it uses an LLM to attempt resolution. The output includes the resolved cultivar, the source of the resolution (direct match, AI, or unresolvable), the raw input, and any assumptions made during resolution.
#' 
#' The output is a list with the following fields:
#' 
#' list(
#'     cultivar = "cultivar_name" | "unknown",
#'     source = "direct_match" | "ai_resolution" | "unresolvable",
#'     raw_input = "farmer_input",
#'     assumptions = NULL
#' )
#' 
#' @param cultivar_input Character string. The cultivar description to resolve.
#' @param crop Character string. The crop for which to resolve the cultivar.
#' @param cultivar_db Data frame. The database of supported cultivars. This is typically retrieved using \code{get_cached_cultivars(crop)} or a data.frame with a "name" column to specify cultivar names.
#' @param use_ai Logical. Whether to use AI fallback if no deterministic match is found
#' @param use_cache Logical. Whether to use caching for AI responses.
#' @return A list with fields: cultivar (resolved cultivar name or "unknown"), source (resolution method), raw_input (original input), and assumptions (any assumptions made during resolution).
#' @examples
#' \dontrun{
#' # Resolve a cultivar with direct match
#' cultivar_db <- data.frame(name = c("scepter", "scout", "gregory"), stringsAsFactors = FALSE)
#' result <- resolve_cultivar("scepter", "wheat", cultivar_db, use_ai = FALSE)
#' 
#' # Resolve with AI fallback for unknown cultivar
#' result <- resolve_cultivar("some local variety", "wheat", cultivar_db, use_ai = TRUE)
#' }
#' @export
resolve_cultivar <- function(
    cultivar_input, 
    crop, 
    cultivar_db = get_cached_cultivars(crop),
    use_ai = TRUE, 
    use_cache = TRUE) {
    stopifnot(is.character(cultivar_input), length(cultivar_input) == 1)
    stopifnot(is.character(crop), length(crop) == 1)
    stopifnot(crop %in% names(agrillm_options$get("crops")))
    stopifnot(is.logical(use_ai), length(use_ai) == 1)
    stopifnot(is.logical(use_cache), length(use_cache) == 1)
    stopifnot(is.data.frame(cultivar_db))    
    stopifnot(all(c("name") %in% names(cultivar_db)))
    raw <- tolower(trimws(cultivar_input))
    supported_cultivars <- unique(tolower(cultivar_db$name))
    
    # 1. Exact / synonym match
    if (raw %in% supported_cultivars) {
        return(list(
            cultivar = supported_cultivars[which(supported_cultivars == raw)],
            source = "direct_match",
            raw_input = cultivar_input,
            assumptions = NULL
        ))
    }
    
    # 3. AI fallback (optional)
    if (!use_ai) {
        warning("Unable to resolve cultivar: ", cultivar_input)
        return(list(
            cultivar = "unknown",
            source = "unresolvable",
            raw_input = cultivar_input,
            assumptions = "LLM fallback disabled and input could not be resolved with deterministic rules"
        )) 
    }
    message("No direct match for cultivar '", cultivar_input, "'. Using AI to attempt resolution...")
    ai_cultivar <- .resolve_cultivar_with_ai(raw, supported_cultivars, use_cache = use_cache)
    return(ai_cultivar)

}


#' Internal: Use LLM to resolve cultivar
#'
#' @param raw_input Character. The user input.
#' @param supported_cultivars Character vector. Supported cultivars.
#' @param use_cache Logical. Whether to use caching for AI responses.
#' @return The best matching cultivar or "unknown".
.resolve_cultivar_with_ai <- function(raw_input, supported_cultivars, use_cache = TRUE) {
    stopifnot(is.character(raw_input), length(raw_input) == 1)
    stopifnot(is.character(supported_cultivars), length(supported_cultivars) > 0)
    
    prompt <- sprintf('
You are mapping a farmer free text cultivar description to a supported cultivar
for an agricultural simulation system.

Supported cultivars: %s

Farmer input: %s

Task:
Return the single best matching cultivar from the supported list.
If none clearly match, return "unknown".

Rules:
- Do not invent new cultivars
- Do not explain
- Return only one word from the list or "unknown"

Example output format:
{
    "cultivar": "ai_cultivar",
    "source": "ai_resolution",
    "raw_input": "raw_input",
    "assumptions": "AI fallback used to resolve cultivar"
}
', 
    paste(supported_cultivars, collapse = ","),
        raw_input
    )
    response <- llm_chat(prompt)
    reply <- tidyllm::get_reply(response)  
    reply <- tolower(trimws(reply))
    # Attempt to parse JSON
    parsed <- tryCatch(jsonlite::fromJSON(reply), error = function(e) NULL)
    if (is.null(parsed)) {
        warning("LLM response could not be parsed as JSON: ", reply)
        return(
            list(
                cultivar = "unknown",
                raw_input = raw_input,
                source = "ai_resolution",
                assumptions = "LLM response was not valid JSON"
            )
        )
    }
    return(parsed)
}

#' Cache APSIM Next Generation cultivar definitions for local use
#'
#' This function caches cultivar definitions from APSIM Next Generation
#' by reading user-supplied APSIM resource files or raw GitHub URLs
#' pointing explicitly to the \code{Models/Resources} directory.
#'
#' The function does not discover, infer, or download APSIM resources on behalf
#' of the user. Instead, users must explicitly supply a valid APSIM resource
#' location (e.g. a raw GitHub URL or equivalent local path) that they are
#' licensed to access. Cached files are stored locally (by default under the
#' project-level \code{.agrillm/} cache) to support reproducibility.
#'
#' Cached cultivar lists are intended for use by \code{resolve_cultivar()} and
#' related functions. No APSIM artefacts are redistributed with the agrillm
#' package.
#'
#' @param apsim_resources_dir Character scalar. A user-supplied path or URL
#'   pointing explicitly to the APSIM Next Generation \code{Models/Resources}
#'   directory (for example, a raw GitHub URL at a specific commit).
#' @param crop Character vector. Crop names for which cultivar definitions
#'   should be cached. If \code{NULL}, all crops defined in
#'   \code{agrillm_options$get("crops")} are processed.
#' @param dest_dir Character scalar. Directory in which cached cultivar files
#'   will be stored. Defaults to
#'   \code{file.path(agrillm_options$get("cache_dir"), "cultivars")}.
#' @param overwrite Logical. If \code{FALSE} (default), existing cached cultivar
#'   files are preserved. If \code{TRUE}, cached files will be replaced.
#'
#' @details
#' This function is deliberately conservative to ensure transparent provenance
#' and licence responsibility. The user is responsible for supplying APSIM
#' resources they are authorised to use and for complying with the APSIM licence.
#'
#' For reproducibility, users are strongly encouraged to reference APSIM
#' resources using a specific commit hash rather than a moving branch
#' (e.g. \code{refs/heads/master}).
#'
#' @return Invisibly returns the cache directory used to store cultivar files.
#'
#' @examples
#' \dontrun{
#' cache_apsim_cultivars(
#'   apsim_resources_dir =
#'     "https://raw.githubusercontent.com/APSIMInitiative/ApsimX/<commit>/Models/Resources",
#'   crop = c("wheat", "barley")
#' )
#' }
#'
#' @seealso \code{\link{resolve_cultivar}}
#'
#' @export
cache_apsim_cultivars <- function(
    apsim_resources_dir,
    crop = NULL,
    dest_dir = NULL,
    overwrite = FALSE
) {
    stopifnot(is.character(apsim_resources_dir), length(apsim_resources_dir) == 1)
    stopifnot(is.logical(overwrite), length(overwrite) == 1)

    # Require explicit raw GitHub Resources directory
    apsim_resources_dir <- gsub("/$", "", apsim_resources_dir) # remove trailing slash if present
    if (!grepl("^https://raw\\.githubusercontent\\.com/.+/Models/Resources$", apsim_resources_dir)) {
        stop(
        "apsim_resources_dir must be a raw GitHub URL to APSIM Models/Resources, e.g.\n",
        "https://raw.githubusercontent.com/APSIMInitiative/ApsimX/<commit>/Models/Resources"
        )
    }

    if (is.null(crop)) {
        crop <- names(agrillm_options$get("crops"))
    }

    if (!is.character(crop)) {
        stop("crop must be a character vector")
    }

    if (is.null(dest_dir)) {
        dest_dir <- file.path(agrillm_options$get("cache_dir"), "cultivars")
    }
    if (!dir.exists(dest_dir)) {
        dir.create(dest_dir, recursive = TRUE)
    }

    for (i in seq_along(crop)) {
        crop_i <- crop[i]
        message("Caching cultivars for crop: ", crop_i)

        resource_url <- sprintf(
            "%s/%s.json",
            apsim_resources_dir,
            tools::toTitleCase(crop_i)
        )

        save_path <- file.path(dest_dir, paste0(tolower(crop_i), "_cultivars.rds"))
        meta_path <- file.path(dest_dir, paste0(tolower(crop_i), "_metadata.json"))

        if (file.exists(save_path) && !overwrite) {
            message("Cultivar cache already exists for '", crop_i, "'. Skipping.")
            next
        }

        model_obj <- rapsimng::read_apsimx(resource_url)
        cultivars <- rapsimng::get_cultivar(model_obj)

        saveRDS(cultivars, save_path)

        metadata <- list(
            crop = crop_i,
            source = "user-supplied APSIM GitHub resource",
            resource_url = resource_url,
            retrieved_at = format(Sys.time(), tz = "UTC"),
            agrillm_version = as.character(utils::packageVersion("agrillm")),
            note = "Derived from APSIM Next Generation resources under user licence"
        )

        jsonlite::write_json(metadata, meta_path, pretty = TRUE, auto_unbox = TRUE)
    }

    invisible(dest_dir)
}

#' Get cached cultivar definitions for a crop
#' 
#' @param crop Character scalar. Crop name for which to retrieve cached cultivars.
#' @return A list of cultivar definitions previously cached by \code{cache_apsim_cultivars()}.
#' @details This function retrieves cultivar definitions that were previously cached by \code{cache_apsim_cultivars()}. If no cached cultivars are found for the specified crop, an error is raised. Users should run \code{cache_apsim_cultivars()} with appropriate APSIM resources before calling this function.
#' @examples
#' \dontrun{
#' get_cached_cultivars("wheat")
#' }
#' @seealso \code{\link{cache_apsim_cultivars}}
#' @export
get_cached_cultivars <- function(crop) {
    stopifnot(is.character(crop), length(crop) == 1)
    crop <- tolower(crop)
    cache_dir <- file.path(agrillm_options$get("cache_dir"), "cultivars")
    cache_path <- file.path(cache_dir, paste0(crop, "_cultivars.rds"))
    if (!file.exists(cache_path)) {
        stop("No cached cultivars found for crop '", crop, "'. Please run cache_apsim_cultivars() first.")
    }
    df <- readRDS(cache_path)
    df
}
