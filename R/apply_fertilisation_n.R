#' Apply fertilisation nitrogen scenarios to an APSIM model
#'
#' Updates the `[Factors].Permutation.Fertilisation` factor in an APSIM Next
#' Generation model. If a measured fertilisation amount is supplied, that value
#' is used directly. Otherwise the default scenario sequence `0, 40, ..., 440` is applied. The function stores the resulting schedule under
#' is retained. Applied evidence is stored under
#' `attr(x, "agrillm")$fertilisation_n`.
#'
#' @param apsimx APSIM model object as returned by `rapsimng::read_apsimx()`.
#' @param fertilisation_n List describing resolved fertilisation nitrogen input,
#'   typically from [resolve_fertilisation_nitrogen()].
#'
#' @return A modified APSIM model object with updated fertilisation scenarios.
#'
#' @examples
#' \dontrun{
#' apsimx_path <- system.file("examples", "wheat.apsimx", package = "agrillm")
#' apsimx <- rapsimng::read_apsimx(apsimx_path)
#' fert_n <- list(value = 80)
#' updated <- apply_fertilisation_n(apsimx, fert_n)
#' attr(updated, "agrillm")$fertilisation_n
#' }
#'
#' @seealso [resolve_fertilisation_nitrogen()]
#' @export
apply_fertilisation_n <- function(apsimx, fertilisation_n) {
    stopifnot(inherits(apsimx, "list"))
    stopifnot(is.list(fertilisation_n))
    amount <- seq(0, 440, by = 40)
    if (!is.null(fertilisation_n$value) && !is.na(fertilisation_n$value) && fertilisation_n$value != "unknown") {
        amount_value <- as.numeric(fertilisation_n$value)
        if (!is.na(amount_value)) {
            amount <- amount_value
        }
    }
    amount <- paste(amount, collapse = ", ")
    
    node_fertilisation <- rapsimng::search_path(apsimx, "[Factors].Permutation.Fertilisation")
    stopifnot(length(node_fertilisation) == 2)

    node_fertilisation$node$Specification <- gsub("^(.*)=.*$", paste0("\\1=", amount), node_fertilisation$node$Specification)
    apsimx <- rapsimng::replace_model(apsimx, node_fertilisation$path, node_fertilisation$node)
    fertilisation_n$evidence <- list(
        amount = amount,
        assumptions = "Assumed fertilisation amount based on provided input. Default values used if input is missing or unknown."
    )
    apsimx <- .add_attribute(apsimx, "fertilisation_n", fertilisation_n)
    return(apsimx)
}
