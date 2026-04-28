
crops_validator <- function(value) {
    if (!is.list(value)) {
        stop("crops must be a list")
    }
    for (i in seq(along = value)) {
        v_crop <- value[[i]]
        if (!is.vector(v_crop)) {
            stop("crops.", names(value)[i], " must be a vector")
        }
        if (!is.character(v_crop)) {
            stop("crops.", names(value)[i], " must be a character vector")
        }
    }
}

llm_validator <- function(value) {
    if (!is.list(value)) {
        stop("llm must be a list")
    }
    if (is.null(value$provider)) {
        stop("llm must have a provider field")
    }
    if (!value$provider %in% names(value)) {
        stop("llm provider must have a corresponding configuration list")
    }
    if (!is.list(value[[value$provider]])) {
        stop("llm provider configuration must be a list")
    }
    if (value$provider == "azure") {
        required_fields <- c("api_version", "resource_name", "deployment_name")
        missing_fields <- setdiff(required_fields, names(value$azure))
        if (length(missing_fields) > 0) {
            stop("Azure configuration is missing fields: ", paste(missing_fields, collapse = ", "))
        }
        if (Sys.getenv("AZURE_OPENAI_API_KEY") == "") {
            stop("AZURE_OPENAI_API_KEY environment variable is not set. Azure LLM calls will fail.")
        }
        if (Sys.getenv("AZURE_ENDPOINT_URL") == "") {
            stop("AZURE_ENDPOINT_URL environment variable is not set. Azure LLM calls will fail.")
        }
    }
}

#' agrillm options
#'
#' An options manager for configuring agrillm parameters. This object provides
#' methods to get and set all related parameters.
#'
#' @section Available Options:
#' \describe{
#'   \item{crops}{List of crops and their associated names. Default: list of wheat, barley, and canola varieties}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{get(key)}{Retrieve the value of an option by its key (e.g., "crops")}
#'   \item{set(key, value)}{Set the value of an option by its key}
#'   \item{reset()}{Reset all options to their default values}
#' }
#'
#' @export
#' @importFrom optree create_options_manager
#' @examples
#' # Get default crops
#' agrillm_options$get("crops")
#' 
#' # Set custom values
#' agrillm_options$set("crops" = list(
#'     wheat = c("wheat", "bread wheat"),
#'     barley = c("barley", "feed barley"),
#'     canola = c("canola", "oilseed rape")
#' ))
#' 
#' # Reset to defaults
#' agrillm_options$reset()
agrillm_options <- optree::create_options_manager(
    defaults = list(
        llm = list(
            provider = "azure",
            azure = list(
                api_version  = "2024-12-01-preview",
                resource_name = "af-systems-ei-zhe00a-agent",
                deployment_name = "gpt-5.2"
            )
        ),
        crops = list(
            wheat   = c("wheat", "bread wheat", "spring wheat", "winter wheat"),
            barley  = c("barley", "feed barley", "malting barley"),
            canola  = c("canola", "oilseed rape", "rapeseed")
        )
    ),
    validators = list(
        "crops" = crops_validator,
        "llm" = llm_validator
    )
)
