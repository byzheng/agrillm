#' Apply resolved sowing dates to an APSIM model
#'
#' Updates the `[Factors].Permutation.SowingDate` factor in an APSIM Next
#' Generation model using a resolved sowing window. The function expands the
#' supplied start and end dates into a comma-separated sequence at 3-day
#' intervals and stores the resulting schedule under
#' `attr(x, "agrillm")$sowing`.
#'
#' @param apsimx APSIM model object as returned by `rapsimng::read_apsimx()`.
#' @param sowing List describing resolved sowing input, typically from
#'   [resolve_sowing()]. Expected to include `start` and `end` as `dd-mon`
#'   strings.
#'
#' @return A modified APSIM model object with updated sowing permutations.
#'
#' @examples
#' \dontrun{
#' apsimx_path <- system.file("examples", "wheat.apsimx", package = "agrillm")
#' apsimx <- rapsimng::read_apsimx(apsimx_path)
#' sowing <- list(start = "01-may", end = "07-may")
#' updated <- apply_sowing(apsimx, sowing)
#' attr(updated, "agrillm")$sowing
#' }
#'
#' @seealso [resolve_sowing()]
#' @export
apply_sowing <- function(apsimx, sowing) {
    stopifnot(inherits(apsimx, "list"))
    stopifnot(is.list(sowing))
    .str_date <- function(x) {
        as.Date(paste(x, "-2011", sep = ""), format = "%d-%b-%Y")
    }
    start_date <- .str_date(sowing$start)
    end_date <- .str_date(sowing$end)    
    sowing_dates <- seq.Date(start_date, end_date, by = "3 day")
    sowing_dates <- format(sowing_dates, "%d-%b") |> tolower() |> paste(collapse = ", ")

    node_sowing <- rapsimng::search_path(apsimx, "[Factors].Permutation.SowingDate")
    stopifnot(length(node_sowing) == 2)

    node_sowing$node$Specification <- gsub("^(.*)=.*$", paste0("\\1=", sowing_dates), node_sowing$node$Specification)
    apsimx <- rapsimng::replace_model(apsimx, node_sowing$path, node_sowing$node)
    sowing$evidence <- list(
        sowing_dates = sowing_dates,
        assumptions = "Assumed sowing dates based on provided start and end dates. Daily sowing dates generated for further analysis."
    )
    apsimx <- .add_attribute(apsimx, "sowing", sowing)
    return(apsimx)
}
