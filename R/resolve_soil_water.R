#' Resolve soil water status from free-text input
#' 
#' @details 
#' 
#' This function attempts to interpret a farmer's free-text description of soil water status using an LLM. It returns a structured representation with PAWC (Plant Available Water Content) in mm, fraction of profile fullness, and a categorical status. If the input cannot be resolved, it returns "unknown".
#' 
#' The output is a list with the following fields:
#' 
#' list(
#'     type = "measured" | "fraction" | "categorical" | "unknown",
#'     pawc_mm = 100,          # optional, only if known
#'     fraction = 1.0,         # optional (0–1)
#'     status = "full",        # categorical
#'     source = "user_provided",
#'     raw_input = "PAWC about 100mm, full profile",
#'     assumptions = NULL
#' )
#' 
#' @param soil_water_input Character string. The soil water status description to resolve.
#' @param use_ai Logical. Whether to use AI fallback if no deterministic match is found
#' @param use_cache Logical. Whether to use caching for AI responses.
#' @return A list with fields: type, pawc_mm, fraction, status,
#' source, raw_input, assumptions. If not resolvable, type is "unknown".
#' @examples
#' # Example with AI backend configured:
#' \dontrun{
#' resolve_soil_water("PAWC about 100mm, full profile")
#' resolve_soil_water("profile about half full")
#' resolve_soil_water("not sure")
#' }
#' @export
resolve_soil_water <- function(soil_water_input, use_ai = TRUE, use_cache = TRUE) {
    raw <- tolower(trimws(soil_water_input))
    message("Resolving soil water status for input: '", soil_water_input, "'")
    if(!use_ai) {
        warning("Unable to resolve soil water status from input: ", soil_water_input)
        return(
            list(
                type = "unknown",
                pawc_mm = NULL,
                fraction = NULL,
                status = NULL,
                source = "unresolvable",
                raw_input = soil_water_input,
                assumptions = "LLM fallback disabled and input could not be resolved with deterministic rules"
            )
        )
    }

    res <-  .resolve_soil_water_with_ai(raw)
    if (!is.null(res) && res$type != "unknown") {
        return(res)
    }
    warning("Unable to resolve soil water status from input: ", soil_water_input)
    return(
        list(
            type = "unknown",
            pawc_mm = NULL,
            fraction = NULL,
            status = NULL,
            source = "ai_resolution",
            raw_input = soil_water_input,
            assumptions = "LLM was unable to interpret the input as a soil water status"
        )
    )
}

#' Internal: Use LLM to resolve soil water status
#' 
#' @param raw_input Character. The user input.
#' @param use_cache Logical. Whether to use caching for AI responses.
#' @return A list with structured soil water fields, or type = "unknown" if not resolvable.
.resolve_soil_water_with_ai <- function(raw_input, use_cache = TRUE) {
    stopifnot(is.character(raw_input), length(raw_input) == 1)
    prompt <- sprintf('
You are extracting soil water information from a farmer\'s free-text input.

Return a JSON object with the following fields ONLY:
- type: "measured", "categorical", or "unknown"
- pawc_mm: number or null
- fraction: number between 0 and 1, or null
- status: "full", "half", "low", or "unknown"
- notes: null


Farmer input:
"%s"

Rules:
- Do NOT invent numbers
- If a number is present, copy it exactly
- If neither PAWC nor profile fullness is known, set type="unknown"
- Return ONLY valid JSON


Example output format:
{
    "type": "measured" | "fraction" | "categorical" | "unknown",
    "pawc_mm": 100,          # optional, only if known
    "fraction": 1.0,         # optional (0–1)
    "status": "full",        # categorical
    "source": "user_provided",
    "raw_input": "PAWC about 100mm, full profile",
    "assumptions": NULL
}
', raw_input)
    response <- llm_chat(prompt, use_cache = use_cache)
    reply <- tidyllm::get_reply(response)  
    parsed <- tryCatch(jsonlite::fromJSON(reply), error = function(e) NULL)
    if (is.null(parsed)) {
        warning("LLM response could not be parsed as JSON: ", reply)
        return(NULL)
    }
    return(parsed)
}