#' Prepare an APSIM simulation file from resolved decisions
#'
#' Reads a template APSIM Next Generation file, applies the resolved decision
#' components needed for simulation setup, and writes the resulting model to a
#' new `.apsimx` file. The assembly currently applies location metadata,
#' default clock settings, weather, soil, cultivar, sowing, soil water, soil
#' nitrogen, and fertilisation nitrogen in sequence.
#'
#' @param decisions List of resolved decision objects. This should include at
#'   least `location`, `crop`, `cultivar`, `sowing`, `soil_water`,
#'   `soil_nitrogen`, and `fert_nitrogen` in the structure expected by the
#'   corresponding `apply_*()` functions.
#' @param template_file Character scalar path to the APSIM template file.
#' @param output_file Character scalar path to the output `.apsimx` file.
#' @param overwrite Logical scalar. If `FALSE` (default), fail when
#'   `output_file` already exists.
#'
#' @return Character scalar path to the written output APSIM file.
#'
#' @examples
#' \dontrun{
#' decisions <- list(
#'   location = list(evidence = list(
#'     weather = list(file = system.file("examples", "wagga.met", package = "agrillm")),
#'     soil = list(file = "path/to/soil.apsimx")
#'   )),
#'   crop = list(crop = "wheat"),
#'   cultivar = list(cultivar = "scepter"),
#'   sowing = list(start = "01-may", end = "07-may"),
#'   soil_water = list(fraction = 0.5),
#'   soil_nitrogen = list(value = 50),
#'   fert_nitrogen = list(value = 80)
#' )
#'
#' prepare_apsimx(
#'   decisions = decisions,
#'   template_file = system.file("examples", "wheat.apsimx", package = "agrillm"),
#'   output_file = tempfile(fileext = ".apsimx"),
#'   overwrite = TRUE
#' )
#' }
#'
#' @export
prepare_apsimx <- function(
    decisions, 
    template_file, 
    output_file, 
    overwrite = FALSE
) {
    stopifnot(is.list(decisions))
    stopifnot(is.character(template_file), length(template_file) == 1, file.exists(template_file))
    stopifnot(is.character(output_file), length(output_file) == 1)
    if (!grepl("\\.apsimx$", output_file, ignore.case = TRUE)) {
        stop("Output file must have .apsimx extension: ", output_file)
    }
    if (file.exists(output_file) && !overwrite) {
        stop("Output file already exists and overwrite is set to FALSE: ", output_file)
    }
    apsimx <- rapsimng::read_apsimx(template_file)
    apsimx <- .add_attribute(apsimx, "location", decisions$location)
    apsimx <- apply_clock(apsimx)
    apsimx <- apply_weather(apsimx, decisions$location$evidence$weather$file)
    apsimx <- apply_soil(apsimx, decisions$location$evidence$soil$file)
    apsimx <- apply_cultivar(apsimx, decisions$crop$crop, decisions$cultivar)
    apsimx <- apply_sowing(apsimx, decisions$sowing)
    apsimx <- apply_soil_water(apsimx, decisions$soil_water)
    apsimx <- apply_soil_nitrogen(apsimx, decisions$soil_nitrogen)
    apsimx <- apply_fertilisation_n(apsimx, decisions$fert_nitrogen)
    rapsimng::write_apsimx(apsimx, output_file)
    return(output_file)
}

