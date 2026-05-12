#' Apply resolved soil nitrogen to an APSIM model
#'
#' Updates the `[Soil].NO3` node in an APSIM Next Generation model using a
#' resolved soil nitrogen input. When a measured total nitrogen amount is
#' supplied, the value is distributed across soil layers using
#' [distribute_soil_n()] and written back to the model. A summary of the
#' applied evidence is attached to the returned object in the
#' `agrillm` attribute under `soil_nitrogen`.
#'
#' @param apsimx APSIM model object as returned by `rapsimng::read_apsimx()`.
#' @param soil_nitrogen List describing resolved soil nitrogen input, typically
#'   from [resolve_soil_nitrogen()]. Measured inputs should include a numeric
#'   `value` field in kg/ha.
#'
#' @return A modified APSIM model object. The returned object includes an
#'   `agrillm` attribute, where `attr(x, "agrillm")$soil_nitrogen` contains
#'   the applied value and assumptions.
#'
#' @examples
#' \dontrun{
#' apsimx_path <- system.file("examples", "wheat.apsimx", package = "agrillm")
#' apsimx <- rapsimng::read_apsimx(apsimx_path)
#' soil_n <- resolve_soil_nitrogen("50 kg/ha", use_ai = FALSE)
#' updated <- apply_soil_nitrogen(apsimx, soil_n)
#' attr(updated, "agrillm")$soil_nitrogen
#' }
#'
#' @seealso [resolve_soil_nitrogen()], [distribute_soil_n()]
#' @export
apply_soil_nitrogen <- function(apsimx, soil_nitrogen) {
    stopifnot(inherits(apsimx, "list"))
    stopifnot(is.list(soil_nitrogen))
    evidence <- list(
        soil_nitrogen = NULL,
        assumptions = NULL
    )
    if (!is.null(soil_nitrogen$value)) {
        amount <- as.numeric(soil_nitrogen$value)
        if (!is.na(amount)) {
            soil_node <- rapsimng::search_path(apsimx, "[Soil]")
            stopifnot(length(soil_node) == 2)
            physical_node <- rapsimng::search_path(apsimx, "[Soil].Physical")
            stopifnot(length(physical_node) == 2)
            thickness <- physical_node$node$Thickness |> unlist() |> as.numeric()
            n_dis <- agrillm::distribute_soil_n(amount, thickness)
            nitrogen_node <- rapsimng::search_path(apsimx, "[Soil].NO3")
            nitrogen_node$node$Thickness <- thickness
            nitrogen_node$node$InitialValues <- n_dis
            nitrogen_node$node$InitialValuesUnits <- 1
            apsimx <- rapsimng::replace_model(apsimx, nitrogen_node$path, nitrogen_node$node)
            evidence$soil_nitrogen <- amount
            evidence$assumptions <- "Assumed soil nitrogen distribution exponential along soil profile."
        }
    } else if (!is.null(soil_nitrogen$type) && soil_nitrogen$type != "unknown") {
        stop("TODO: implement soil nitrogen profile to soil module")
    }
    apsimx <- .add_attribute(apsimx, "soil_nitrogen", evidence)
    return(apsimx)
}


