
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


