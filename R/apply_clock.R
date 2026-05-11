#' Apply a simulation clock to an APSIM model
#'
#' Updates the `[Clock]` node in an APSIM Next Generation model to set the
#' simulation start and end timestamps. Supporting evidence is stored under
#' `attr(x, "agrillm")$clock`.
#'
#' @param apsimx APSIM model object as returned by `rapsimng::read_apsimx()`.
#' @param start Character scalar start timestamp in `YYYY-mm-ddTHH:MM:SS`
#'   format.
#' @param end Character scalar end timestamp in `YYYY-mm-ddTHH:MM:SS` format.
#'
#' @return A modified APSIM model object with updated clock settings.
#'
#' @examples
#' \dontrun{
#' apsimx_path <- system.file("templates", "wheat.apsimx", package = "agrillm")
#' apsimx <- rapsimng::read_apsimx(apsimx_path)
#' updated <- apply_clock(apsimx, start = "2012-01-01T00:00:00", end = "2013-12-31T00:00:00")
#' attr(updated, "agrillm")$clock
#' }
#'
#' @export
apply_clock <- function(
    apsimx, 
    start = "1976-01-01T00:00:00", 
    end = "2025-12-31T00:00:00"
) {
    stopifnot(inherits(apsimx, "list"))
    stopifnot(is.character(start), length(start) == 1)
    stopifnot(is.character(end), length(end) == 1)
    stopifnot(!is.na(as.POSIXct(start, format = "%Y-%m-%dT%H:%M:%S")))
    stopifnot(!is.na(as.POSIXct(end, format = "%Y-%m-%dT%H:%M:%S")))
    
    node_clock <- rapsimng::search_path(apsimx, "[Clock]")
    node_clock$node$Start <- start
    node_clock$node$End <- end
    apsimx <- rapsimng::replace_model(apsimx, node_clock$path, node_clock$node)
    att_clock <- list(evidence = list(
        clock = list(
            start = start,
            end = end
        )),
        assumptions = "Default simulation period applied (1976-2025) to capture historical weather patterns."
    )
    apsimx <- .add_attribute(apsimx, "clock", att_clock)
    return(apsimx)
}
