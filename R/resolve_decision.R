#' Resolve user input to a structured decision category, optionally using AI for interpretation.
#' 
#' This function takes a free-text description of a decision (e.g. "sowing time choice", "cultivar choice", "yield risk assessment", "fertilisation nitrogen choice") and attempts to classify it into one of the allowed decision categories. It uses deterministic rules where possible, and falls back to an LLM-based classification if enabled. The output includes the resolved decision type, any extracted year range, the source of the resolution, and any assumptions made during interpretation.
#' 
#' @param decision_input Character string. The free-text description of the decision to resolve.
#' @param use_ai Logical. Whether to use AI fallback if deterministic rules do not yield
#' a clear resolution. Default: TRUE.
#' @param use_cache Logical. Whether to use caching for AI responses. Default: TRUE
#' @return A list with fields: type (decision category), year_start, year_end, source (resolution method), raw_input, assumptions. If the input cannot be resolved, type will be "unknown".
#' @examples
#' # With AI fallback enabled:
#' # resolve_decision("What about sowing time choice?")
#' # resolve_decision("I'm thinking about cultivar choice for the next season.")
#' # resolve_decision("How does the yield risk assessment look?")
#' 
#' @export
resolve_decision <- function(decision_input, use_ai = TRUE, use_cache = TRUE) {
    stopifnot(is.logical(use_ai), length(use_ai) == 1)
    stopifnot(is.logical(use_cache), length(use_cache) == 1)
    stopifnot(is.character(decision_input) || is.null(decision_input), length(decision_input) == 1)

    raw <- tolower(trimws(decision_input))
    message("Resolving decision for input: '", decision_input, "'")
    if(!use_ai) {
        warning("Unable to resolve decision from input: ", decision_input)
        return(
            list(
                type = "unknown",
                year_start = NULL,
                year_end = NULL,
                source = "unresolvable",
                raw_input = decision_input,
                assumptions = "LLM fallback disabled and input could not be resolved with deterministic rules"
            )
        )
    }
    res <-  .resolve_decision_with_ai(raw, use_cache = use_cache)
    if (res$type != "unknown") {
        return(res)
    }
    warning("Unable to resolve decision from input: ", decision_input)
    return(
        list(
            type = "unknown",
            year_start = NULL,
            year_end = NULL,
            source = "ai_resolution",
            raw_input = raw,
            assumptions = "LLM was unable to interpret the input as a known decision"
        )
    )
}


#' Internal: Use LLM to resolve decision category
#' 
#' @param raw_input Character. The user input.
#' @param use_cache Logical. Whether to use caching for AI responses.
#' @return A list with structured decision fields, or type = "unknown" if not resolvable.
.resolve_decision_with_ai <- function(raw_input, use_cache = TRUE) {
    stopifnot(is.character(raw_input), length(raw_input) == 1)
    stopifnot(is.logical(use_cache), length(use_cache) == 1)

    allowed <- c(
        "sowing_time_choice",
        "cultivar_choice",
        "yield_risk_assessment",
        "fertilisation_nitrogen_choice",
        "unknown"
    )

    # Deterministic time anchor (avoid LLM guessing "current year")
    analysis_year_end <- as.integer(format(Sys.Date(), "%Y")) - 1L

    prompt <- sprintf(
'You are classifying user decision intent.

Your task is to map informal user inputs to ONE decision category from the allowed list.

You must NOT:
- propose scenarios
- infer agronomic thresholds
- generate recommendations
- invent new decision categories

Allowed decision categories:
%s

If the user mentions years/decades/periods, extract a numeric year range when possible.
Use analysis_year_end = %d as the current (most recent complete) year.

Examples:
- "2010s" -> year_start: 2010, year_end: 2019
- "1990-2005" -> year_start: 1990, year_end: 2005
- "last 20 years" -> year_start: analysis_year_end - 19, year_end: analysis_year_end
- "since 2000" -> year_start: 2000, year_end: analysis_year_end

Return ONLY valid JSON with EXACTLY these keys:
{
  "type": "<one of allowed decision categories>",
  "year_start": <integer or null>,
  "year_end": <integer or null>,
  "assumptions": <string or null>
}

If input cannot be interpreted, return:
{"type":"unknown","year_start":null,"year_end":null,"assumptions":null}

User input:
%s
',
        paste(sprintf("- %s", allowed), collapse = "\n"),
        analysis_year_end,
        raw_input
    )

    response <- llm_chat(prompt, use_cache = use_cache)
    reply <- tidyllm::get_reply(response)

    parsed <- tryCatch(jsonlite::fromJSON(reply), error = function(e) NULL)

    # Helper fallback
    fallback <- function(msg) {
        list(
        type = "unknown",
        year_start = NULL,
        year_end = NULL,
        source = "ai_resolution",
        raw_input = raw_input,
        assumptions = msg
        )
    }

    if (is.null(parsed)) {
        warning("LLM response could not be parsed as JSON: ", reply)
        return(fallback("LLM response was not valid JSON"))
    }

    # Normalise / validate
    if (is.null(parsed$type) || !is.character(parsed$type) || length(parsed$type) != 1) {
        return(fallback("Missing or invalid 'type' field"))
    }
    if (!parsed$type %in% allowed) {
        return(fallback("Returned type not in allowed categories"))
    }

    # Ensure keys exist
    if (is.null(parsed$year_start)) parsed$year_start <- NULL
    if (is.null(parsed$year_end)) parsed$year_end <- NULL
    if (is.null(parsed$assumptions)) parsed$assumptions <- NULL

    # Coerce years to integer if present
    if (!is.null(parsed$year_start)) parsed$year_start <- as.integer(parsed$year_start)
    if (!is.null(parsed$year_end)) parsed$year_end <- as.integer(parsed$year_end)

    # Validate year logic
    if (!is.null(parsed$year_start) && !is.null(parsed$year_end)) {
        if (parsed$year_start > parsed$year_end) {
        return(fallback("year_start > year_end"))
        }
    }

    parsed$source <- "ai_resolution"
    parsed$raw_input <- raw_input
    if (is.null(parsed$assumptions)) {
        parsed$assumptions <- "LLM classification using allowed categories and provided year rules"
    }

    parsed
}
