resolve_cultivar <- function(
    cultivar_input, 
    crop, 
    cultivar_db = NULL,
    use_ai = TRUE, 
    use_cache = TRUE) {
    stopifnot(is.character(cultivar_input), length(cultivar_input) == 1)
    stopifnot(is.character(crop), length(crop) == 1)
    stopifnot(crop %in% names(agrillm_options$get("crops")))
    stopifnot(is.logical(use_ai), length(use_ai) == 1)
    stopifnot(is.logical(use_cache), length(use_cache) == 1)
    

    url_path <- paste0("crops/", crop, "/cultivars")
    known_cultivars <- rapsimng::get_cultivars(crop)
}



cache_apsim_cultivars <- function(
    crop = NULL,
    source = "APSIM-GitHub",
    url,
    dest_dir = "~/.agrillm/cultivars",
    overwrite = FALSE
) {
    if (is.null(crop)) {
        crop <- agrillm_options$get("crops")
    }
    if (missing(crop) || !is.character(crop) || length(crop) != 1) {
        stop("Crop must be a single character string")
    }
}
