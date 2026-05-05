
#' Resolve a location to coordinates
#'
#' Resolves a user-provided location to latitude/longitude. Prefer explicit
#' coordinates when available; optionally fall back to AI/geocoding rules if needed.
#'
#' @param location_input Character. Free-text location description (e.g. "Wagga Wagga, NSW").
#' @param latitude Numeric. Latitude if already known.
#' @param longitude Numeric. Longitude if already known.
#' @param use_ai Logical. Whether to use AI fallback when no coordinates are provided.
#' @param require_coords Logical. If TRUE, error when coordinates cannot be resolved.
#'
#' @return A list with latitude, longitude, display_name, source, confidence, raw_input, assumptions.
#' @export
resolve_location <- function(
    location_input = NULL, latitude = NULL, longitude = NULL,
    use_ai = TRUE, require_coords = TRUE) {

    # If coords already provided, accept them
    if (!is.null(latitude) && !is.null(longitude)) {
        stopifnot(is.numeric(latitude), is.numeric(longitude))
        stopifnot(length(latitude) == 1, length(longitude) == 1)
        stopifnot(latitude >= -90, latitude <= 90, longitude >= -180, longitude <= 180)
        return(list(
            latitude = as.numeric(latitude),
            longitude = as.numeric(longitude),
            display_name = if (!is.null(location_input)) location_input else NULL,
            source = "provided_coordinates",
            confidence = "high",
            raw_input = location_input,
            assumptions = NULL
        ))
    }

    # If no coords, attempt deterministic lookup (optional)
    if (!is.null(location_input)) {
        raw <- trimws(location_input)

        # (Optional) deterministic gazetteer lookup could go here
        # match <- lookup_gazetteer(raw)

        if (use_ai) {
            loc <- .resolve_location_with_ai(raw)
            if (!is.null(loc$latitude) && !is.null(loc$longitude)) {
                return(loc)
            }
        }
    }

    if (require_coords) {
        stop("Unable to resolve location to coordinates: ", location_input %||% "<NULL>")
    }

    list(
        latitude = NULL, longitude = NULL,
        display_name = NULL,
        source = "unknown",
        confidence = "low",
        raw_input = location_input,
        assumptions = "Location unresolved"
    )
}




#' Internal: Use LLM to resolve location
#'
#' @param raw_input Character. The user input.
#' @return A list with latitude, longitude, display_name, source, confidence, raw_input, assumptions.
.resolve_location_with_ai <- function(raw_input) {
    stopifnot(is.character(raw_input), length(raw_input) == 1)
    prompt <- sprintf('

You are extracting a canonical Australian place name from a farmer\'s free-text location.

Farmer input:
"%s"

Task:

Return a single place name (town or well-known region) suitable for geocoding.

If you cannot identify a place, return "unknown".

Rules:

- Do not explain
- Return ONLY valid JSON for latitude, longitude and display_name
- If unsolved return unknown {"type":"unknown"}

Example output format:
{
    latitude = -20,
    longitude = 111,
    display_name = Wagga Wagga,
    source = "ai_resolution",
    confidence = "low",
    raw_input = "%s",
    assumptions = "Location is likely in NSW based on common farming regions"
}
    ', raw_input, raw_input)

    response <- llm_chat(prompt)
    reply <- tidyllm::get_reply(response)
    # Attempt to parse JSON
    parsed <- tryCatch(jsonlite::fromJSON(reply), error = function(e) NULL)
    if (is.null(parsed)) {
        warning("LLM response could not be parsed as JSON: ", reply)
        return(
            list(
                type = "unknown",
                raw_input = raw_input,
                source = "ai_resolution",
                assumptions = "LLM response was not valid JSON"
            )
        )
    }
    return(parsed)
}
