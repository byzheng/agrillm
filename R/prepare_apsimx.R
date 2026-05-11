
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

