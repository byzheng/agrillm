#' Unified LLM chat interface for agrillm
#'
#' @param prompt The prompt or message to send to the LLM.
#' @param use_cache Logical. If `TRUE`, reuse cached backend responses for
#' identical requests within the current R session. Default: `FALSE`.
#' @param persist_cache Logical. If `TRUE`, persist cached responses to disk so
#' they can be reloaded in later sessions. Default: `FALSE`.
#' @param cache_path Optional character string path to the cache file used when
#' `persist_cache = TRUE`. Default: `.cache_agrillm.rds` in the current working
#' directory.
#' @param ... Additional arguments passed to the backend chat function.
#' @return The response from the LLM backend.
#' @export
llm_chat <- function(prompt, use_cache = FALSE, persist_cache = FALSE, cache_path = NULL, ...) {
    stopifnot(is.character(prompt), length(prompt) == 1)
    stopifnot(is.logical(use_cache), length(use_cache) == 1, !is.na(use_cache))
    stopifnot(is.logical(persist_cache), length(persist_cache) == 1, !is.na(persist_cache))
    if (!is.null(cache_path)) {
        stopifnot(is.character(cache_path), length(cache_path) == 1, nzchar(cache_path))
    }
    llm_opts <- agrillm_options$get("llm")
    provider <- llm_opts$provider
    if (provider == "azure") {
        # Use the high-level Azure chat wrapper
        return(azure_chat(
            prompt,
            use_cache = use_cache,
            persist_cache = persist_cache,
            cache_path = cache_path,
            ...
        ))
    }
    stop(sprintf("LLM provider '%s' is not supported yet.", provider))
}
