#' Unified LLM chat interface for agrillm
#'
#' @param prompt The prompt or message to send to the LLM.
#' @param ... Additional arguments passed to the backend chat function.
#' @return The response from the LLM backend.
#' @export
llm_chat <- function(prompt, ...) {
    llm_opts <- agrillm_options$get("llm")
    provider <- llm_opts$provider
    if (provider == "azure") {
        # Use the high-level Azure chat wrapper
        return(azure_chat(prompt, ...))
    }
    stop(sprintf("LLM provider '%s' is not supported yet.", provider))
}
