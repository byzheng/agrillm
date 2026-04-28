
crops_validator <- function(value) {
    if (!is.list(value)) {
        stop("crops must be a list")
    }
    for (i in seq(along = value)) {
        optree::v_character_scalar()
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
        crops = list(
            wheat   = c("wheat", "bread wheat", "spring wheat", "winter wheat"),
            barley  = c("barley", "feed barley", "malting barley"),
            canola  = c("canola", "oilseed rape", "rapeseed")
        )
    ),
    validators = list(
        "crops" = crops_validator
    )
)
