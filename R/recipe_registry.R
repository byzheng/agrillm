#' List available recipe interfaces
#'
#' Returns high-level metadata describing each recipe_* function,
#' intended for AI-assisted reporting and auditing.
#'
#' @return A list of recipe interface descriptions.
#' @export
recipe_registry <- function() {
    as.list(.recipe_registry_env)
}
