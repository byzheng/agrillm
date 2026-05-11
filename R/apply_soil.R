#' Apply a replacement soil profile to an APSIM model
#'
#' Replaces the existing `[Soil]` node in an APSIM Next Generation model using
#' a second APSIM file that contains a prepared soil configuration.
#'
#' @param apsimx APSIM model object as returned by `rapsimng::read_apsimx()`.
#' @param file Character scalar path to an APSIM file containing the soil node
#'   to insert.
#'
#' @return A modified APSIM model object with the replacement soil node.
#'
#' @examples
#' \dontrun{
#' apsimx_path <- system.file("templates", "wheat.apsimx", package = "agrillm")
#' apsimx <- rapsimng::read_apsimx(apsimx_path)
#' updated <- apply_soil(apsimx, apsimx_path)
#' }
#'
#' @export
apply_soil <- function(apsimx, file) {
    stopifnot(inherits(apsimx, "list"))
    stopifnot(is.character(file), length(file) == 1, file.exists(file))
    
    new_soil <- rapsimng::read_apsimx(file)
    new_soil$Name <- "Soil"
    node_soil <- rapsimng::search_path(apsimx, "[Soil]")

    apsimx <- rapsimng::replace_model(apsimx, node_soil$path, new_soil)
    return(apsimx)
}
