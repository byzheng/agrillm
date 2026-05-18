#' Apply resolved soil water to an APSIM model
#'
#' Updates the `[Soil].Water` node in an APSIM Next Generation model using a
#' resolved soil water input. When a fraction of plant available water capacity
#' is supplied, the function calculates layer initial water contents between
#' `LL15` and `DUL`, writes them into the model, and stores supporting evidence
#' under `attr(x, "agrillm")$soil_water`.
#'
#' @param apsimx APSIM model object as returned by `rapsimng::read_apsimx()`.
#' @param soil_water List describing resolved soil water input, typically from
#'   [resolve_soil_water()]. Supported inputs currently include a `fraction`
#'   field between 0 and 1. If fraction is missing or invalid, the function 
#'   assumes the full soil profile is at DUL.
#'
#' @return A modified APSIM model object with updated soil water state.
#'
#' @examples
#' \dontrun{
#' apsimx_path <- system.file("examples", "wheat.apsimx", package = "agrillm")
#' apsimx <- rapsimng::read_apsimx(apsimx_path)
#' water <- resolve_soil_water("profile about half full", use_ai = FALSE)
#' updated <- apply_soil_water(apsimx, water)
#' attr(updated, "agrillm")$soil_water
#' }
#'
#' @seealso [resolve_soil_water()]
#' @export
apply_soil_water <- function(apsimx, soil_water) {
    stopifnot(inherits(apsimx, "list"))
    stopifnot(is.list(soil_water))
    evidence <- list(
        soil_water = NULL,
        assumptions = NULL
    )
    if (!is.null(soil_water$pawc_mm)) {
        stop("TODO: impleent PAWC to soil profile")
    }
    frac <- 1
    if (is.null(soil_water$fraction)) {
        warning("No initial soil water information is provided. Assume the full soil profile.")
    } else {
        frac <- as.numeric(soil_water$fraction)
        if (is.na(frac) || frac < 0 || frac > 1) {
            warning("Invalid soil water fraction provided: ", soil_water$fraction, ". Must be between 0 and 1. Assuming full profile.")
            frac <- 1
        }
    }

    soil_node <- rapsimng::search_path(apsimx, "[Soil]")
    stopifnot(length(soil_node) == 2)
    physical_node <- rapsimng::search_path(apsimx, "[Soil].Physical")
    stopifnot(length(physical_node) == 2)
    water_node <- rapsimng::search_path(apsimx, "[Soil].Water")
    stopifnot(length(water_node) == 2)
    depth <- physical_node$node$Thickness |> unlist() |> as.numeric()
    dul <- physical_node$node$DUL |> unlist() |> as.numeric()
    ll15 <- physical_node$node$LL15 |> unlist() |> as.numeric()
    sw <- ll15 + frac * (dul - ll15)
    water_node$node$InitialValues <- sw        
    water_node$node$Thickness <- depth
    apsimx <- rapsimng::replace_model(apsimx, water_node$path, water_node$node)
    evidence$soil_water <- sum((sw - ll15) * depth)
    evidence$assumptions <- "Assumed soil water content based on provided fraction of PAWC."
    apsimx <- .add_attribute(apsimx, "soil_water", evidence)
    return(apsimx)
}
