#' Resolve soil nitrogen input
#' 
#' @param soil_n_input Character or numeric. User input describing soil nitrogen status (e.g. "low", "moderate", "high", "50 kg/ha").
#' @param use_ai Logical. Whether to use AI fallback if input cannot be resolved with deterministic rules. Default: TRUE.
#' @param use_cache Logical. Whether to use caching for AI responses. Default: TRUE
#' @return A list with fields: type ("measured", "categorical", "unknown"), value (numeric if measured), status (categorical if categorical), unit, source, raw_input, assumptions.
#' @examples
#' 
#' \dontrun{
#' resolve_soil_nitrogen("low")
#' resolve_soil_nitrogen("50 kg/ha")
#' resolve_soil_nitrogen("not sure")
#' }
#' @export
resolve_soil_nitrogen <- function(soil_n_input, use_ai = TRUE, use_cache = TRUE) {
    stopifnot(is.logical(use_ai), length(use_ai) == 1) 
    stopifnot(is.logical(use_cache), length(use_cache) == 1)
    stopifnot(is.character(soil_n_input) || is.numeric(soil_n_input) || is.null(soil_n_input), length(soil_n_input) == 1)
    # 1. Handle missing / skipped
    if (is.null(soil_n_input) || is.na(soil_n_input) || soil_n_input == "") {
        return(list(
            type = "unknown",
            source = "default",
            raw_input = soil_n_input,
            assumptions = "Soil nitrogen assumed at regional typical level"
        ))
    }

    raw <- tolower(trimws(soil_n_input))

    # 2. Numeric value detected (kg/ha)
    value_match <- regexpr("\\d+(\\.\\d+)?", raw)
    if (value_match > 0) {
        value <- as.numeric(regmatches(raw, value_match))

        return(list(
            type = "measured",
            value = value,
            unit = "kg/ha",
            source = "user_provided",
            raw_input = soil_n_input,
            assumptions = "Assumed to represent total profile mineral N at sowing"
        ))
    }

    # 3. Qualitative categories
    if (raw %in% c("low", "low n", "nitrogen poor")) {
        return(list(type = "categorical", status = "low",
                    source = "direct_match", raw_input = soil_n_input))
    }

    if (raw %in% c("moderate", "medium", "background", "background n only", "reasonable")) {
        return(list(type = "categorical", status = "moderate",
                    source = "direct_match", raw_input = soil_n_input))
    }

    if (raw %in% c("high", "plenty", "plenty of nitrogen", "high n")) {
        return(list(type = "categorical", status = "high",
                    source = "direct_match", raw_input = soil_n_input))
    }

    # 4. Explicit uncertainty
    if (raw %in% c("not sure", "unknown", "unsure")) {
        return(list(
        type = "unknown",
        source = "user_unknown",
        raw_input = soil_n_input,
        assumptions = "Soil nitrogen assumed at regional typical level"
        ))
    }

    # 5. AI fallback (optional)
    if (!use_ai) {
        warning("Unable to resolve soil nitrogen input: ", soil_n_input)
        return(list(
            type = "unknown",
            source = "unresolvable",
            raw_input = soil_n_input,
            assumptions = "LLM fallback disabled and input could not be resolved with deterministic rules"
        ))
    }
    ai_status <- .resolve_soil_n_with_ai(raw, use_cache = use_cache)
    return(ai_status)
}



#' Internal: Use LLM to resolve soil nitrogen
#'
#' @param raw_input Character. The user input.
#' @param use_cache Logical. Whether to use caching for AI responses.
#' @return The best matching crop or "unknown".
.resolve_soil_n_with_ai <- function(raw_input, use_cache = TRUE) {
    stopifnot(is.character(raw_input), length(raw_input) == 1)
    
    prompt <- sprintf('
You are mapping a farmer\'s free-text description of soil nitrogen
to a categorical nitrogen status.

Allowed outputs:
- low
- moderate
- high
- unknown

Farmer input:
"%s"

Rules:
- Do not return numbers
- Do not explain
- Return only one word from the allowed outputs


Example output format:
{
    "type": "categorical",
    "status": "low",
    "source": "ai_resolution",
    "raw_input": raw_input,
    "assumptions": "Soil nitrogen assumed at regional typical level" or NULL
}

', 
    raw_input
    )
    response <- llm_chat(prompt, use_cache = use_cache)
    reply <- tidyllm::get_reply(response)  
    reply <- tolower(trimws(reply))
    
    parsed <- tryCatch(jsonlite::fromJSON(reply), error = function(e) NULL)
    if (is.null(parsed)) {
        warning("LLM response could not be parsed as JSON: ", reply)
        return(
            list(
                type = "unknown",
                status = "unknown",
                source = "ai_resolution",
                raw_input = raw_input,
                assumptions = NULL
            )
        )
    }
    return(parsed)
}