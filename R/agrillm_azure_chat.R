.azure_chat <- function(message, deployment, api_version, ...) {
    tidyllm::azure_openai_chat(
        message,
        .deployment = deployment,
        .api_version = api_version,
        ...
    )
}

#' High-level function to call Azure OpenAI chat using tidyllm
#'
#' @param message A character string with the prompt or message.
#' @param ... Additional arguments passed to azure_openai_chat.
#' @return The response from Azure OpenAI chat.
#' @export
#' @examples
#' \dontrun{
#' msg <- llm_message("Test prompt")
#' response <- azure_chat(msg)
#' print(response)
#' }
azure_chat <- function(message, ...) {
    llm_opts <- agrillm_options$get("llm")
    if (llm_opts$provider != "azure") {
        stop("Only Azure provider is supported in this function.")
    }
    # Extract Azure config
    deployment <- llm_opts$azure$deployment_name
    api_version <- llm_opts$azure$api_version

    .azure_chat(
        message,
        deployment = deployment,
        api_version = api_version,
        ...
    )
}
