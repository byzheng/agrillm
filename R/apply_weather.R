
#' Apply a weather file to an APSIM model
#'
#' Updates the `[Weather]` node in an APSIM Next Generation model so the
#' `FileName` field points to the supplied weather file name.
#'
#' @param apsimx APSIM model object as returned by `rapsimng::read_apsimx()`.
#' @param file Character scalar path to a weather file.
#'
#' @return A modified APSIM model object with updated weather file reference.
#'
#' @examples
#' \dontrun{
#' apsimx_path <- system.file("templates", "wheat.apsimx", package = "agrillm")
#' met_path <- system.file("templates", "wagga.met", package = "agrillm")
#' apsimx <- rapsimng::read_apsimx(apsimx_path)
#' updated <- apply_weather(apsimx, met_path)
#' }
#'
#' @export
apply_weather <- function(apsimx, file) {
    stopifnot(inherits(apsimx, "list"))
    stopifnot(is.character(file), length(file) == 1, file.exists(file))

    node_weather <- rapsimng::search_path(apsimx, "[Weather]")
    node_weather$node$FileName <- basename(file)
    apsimx <- rapsimng::replace_model(apsimx, node_weather$path, node_weather$node)
    return(apsimx)
}