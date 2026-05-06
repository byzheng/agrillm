#' Resolve a sowing window from free-text input
#'
#' This function attempts to interpret a farmer's free-text description of a sowing window using a combination of deterministic rules and an LLM fallback (only AI is implemented now). It returns a structured sowing window with start/end dates and any assumptions made.
#' 
#' @param sowing_input Character string. The sowing window description to resolve.
#' @param use_ai Logical. Whether to use AI fallback if no deterministic match is found. Default: TRUE.
#' @param use_cache Logical. Whether to use caching for AI responses. Default: TRUE.
#' @return A list with fields: type, start, end, shift, source, raw_input, assumptions. If not resolvable, type is "unknown".
#' @examples
#' # Example with AI backend configured:
#' # resolve_sowing("early May")
#' # resolve_sowing("mid June")
#' # resolve_sowing("unknown phrase")
#' @export
resolve_sowing <- function(sowing_input, use_ai = TRUE, use_cache = TRUE) {
    stopifnot(is.logical(use_ai), length(use_ai) == 1)
    stopifnot(is.logical(use_cache), length(use_cache) == 1)
    stopifnot(is.character(sowing_input) || is.null(sowing_input), length(sowing_input) == 1)

    raw <- tolower(trimws(sowing_input))
    message("Resolving sowing window for input: '", sowing_input, "'")
    if(!use_ai) {
        warning("Unable to resolve sowing window from input: ", sowing_input)
        return(
            list(
                type = "unknown",
                start  = NULL,
                end = NULL,
                shift = NULL,
                source = "unresolvable",
                raw_input = sowing_input,
                assumptions = "LLM fallback disabled and input could not be resolved with deterministic rules"
            )
        )
    }
    res <-  .resolve_sowing_with_ai(raw)
    if (res$type != "unknown") {
        return(res)
    }
    warning("Unable to resolve sowing window from input: ", sowing_input)
    return(
        list(
            type = "unknown",
            start  = NULL,
            end = NULL,
            shift = NULL,
            source = "ai_resolution",
            raw_input = sowing_input,
            assumptions = "LLM was unable to interpret the input as a sowing window"
        )
    )
}

#' Internal: Use LLM to resolve sowing window
#'
#' @param raw_input Character. The user input.
#' @param use_cache Logical. Whether to use caching for AI responses.
#' @return A list with structured sowing window fields, or type = "unknown" if not resolvable.
.resolve_sowing_with_ai <- function(raw_input, use_cache = TRUE) {
    stopifnot(is.character(raw_input), length(raw_input) == 1)
    prompt <- sprintf('

You are converting a farmer free-text sowing description into a structured sowing window for an agricultural decision-support system.

Farmer input:

%s

Task:

Return a JSON object with the following fields:

- type: always "window"
- start: start of sowing window in DD-BBB (e.g., "01-May")
- end: end of sowing window in DD-BBB (e.g., "10-May")
- shift: one of "before", "mid", "after"
- source: one of "user_phrase"
- raw_input: copy the original input exactly
- assumptions: a string describing any assumptions you made in interpreting the input, or null if none

Interpretation rules (use common Australian grain-farming conventions):

A general rule for a period time:

- "early <month>" to days 01-10, shift as "mid"
- "mid <month>" to days 11-20, shift as "mid"
- "late <month>" to days 21-end, shift as "mid"

Before a specifc day can be indicated with "before <day> <month>"

-  which should set the shift to "before" respectively
- "end" as the specified day
- "start" as 10 days before the specified day

After a specifc day can be indicated with "after <day> <month>"

- which should set the shift to "after" respectively
- "start" as the specified day
- "end" as 10 days after the specified day

As soon as we get a break 

- which should set the broad window from 1 April (start) to 31 July (end)
- shift should be "before"
- assumptions should note that this is a broad interpretation based on the presence of "break" in the input and typical sowing periods for Australian grains

As soon as before June

- which should set the end to 31 May and shift to "before"
- start can be set to 1 April as a reasonable assumption for the earliest sowing date for many Australian grains
- assumptions should note that this is a broad interpretation based on the presence of "before June" in the input and typical sowing periods for Australian grains



Constraints:

- Do not include a year
- Do not explain your reasoning
- Return ONLY valid JSON
- If the input cannot be interpreted as a month, return:
  {"type":"unknown"}

Example output format:
{
    "type": "window",
    "start": "05-01",
    "end": "05-10",
    "shift": "before",
    "source": "user_phrase",
    "raw_input": "early May",
    "assumptions": null
}     
    ', raw_input)
    response <- llm_chat(prompt, use_cache = use_cache)
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