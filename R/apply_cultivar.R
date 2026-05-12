
#' Apply cultivar scenarios to an APSIM model
#'
#' Updates the `[Factors].Permutation.Cultivar` factor in an APSIM Next
#' Generation model using resolved cultivar input. If cultivar information is
#' missing or unknown, the function falls back to a built-in generic cultivar
#' list and records that assumption under `attr(x, "agrillm")$cultivar`.
#'
#' @param apsimx APSIM model object as returned by `rapsimng::read_apsimx()`.
#' @param crop Character scalar naming the crop associated with the cultivar
#'   input.
#' @param cultivar List describing resolved cultivar input, typically from
#'   [resolve_cultivar()].
#'
#' @return A modified APSIM model object with updated cultivar permutations.
#'
#' @examples
#' \dontrun{
#' apsimx_path <- system.file("examples", "wheat.apsimx", package = "agrillm")
#' apsimx <- rapsimng::read_apsimx(apsimx_path)
#' cultivar <- list(cultivar = "scepter")
#' updated <- apply_cultivar(apsimx, crop = "wheat", cultivar = cultivar)
#' attr(updated, "agrillm")$cultivar
#' }
#'
#' @seealso [resolve_cultivar()]
#' @export
apply_cultivar <- function(apsimx, crop, cultivar) {
    stopifnot(inherits(apsimx, "list"))
    stopifnot(is.list(cultivar))
    stopifnot(!is.null(cultivar$cultivar))
    stopifnot(is.character(crop), length(crop) == 1)

    cultivar_list <- cultivar$cultivar
    evidence <- list(
        cultivar = cultivar_list,
        assumptions = NULL
    )
    if (is.null(cultivar_list) || is.na(cultivar_list) || cultivar_list == "" || cultivar_list == "unknown") {
        cultivar_list <- c(
            "Spring Very Quick-Quick", "Spring Quick",           
            "Spring Quick-Mid",        "Spring Mid",             
            "Spring Mid-Slow",         "Spring Slow",            
            "Spring Slow-Very Slow",   "Spring Very Slow",       
            "Winter Very-Quick",       "Winter Quick",           
            "Winter Mid",              "Winter Very Slow"
        ) |> tolower()
        warning(
            "Cultivar information is missing or unknown. Applying generic cultivar list for crop '", crop, "':\n",
            paste0("- ", cultivar_list, collapse = "\n")
        )
        evidence <- list(
            cultivar = cultivar_list,
            assumptions = "Cultivar information is missing or unknown. Applied generic cultivar list."
        )
    }
    node_cultivar <- rapsimng::search_path(apsimx, "[Factors].Permutation.Cultivar")
    stopifnot(length(node_cultivar) == 2)

    cultivar_list <- paste(cultivar_list, collapse = ", ")
    node_cultivar$node$Specification <- gsub("^(.*)=.*$", paste0("\\1=", cultivar_list), node_cultivar$node$Specification)
    apsimx <- rapsimng::replace_model(apsimx, node_cultivar$path, node_cultivar$node)
    cultivar$evidence <- evidence
    apsimx <- .add_attribute(apsimx, "cultivar", cultivar)
    return(apsimx)
}
