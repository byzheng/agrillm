#' Resolve a sowing window from free-text input
#'
#' This function attempts to interpret a farmer's free-text description of a sowing window using a combination of deterministic rules and an LLM fallback (only AI is implemented now). It returns a structured sowing window with start/end dates and any assumptions made.
#' 
#' @param sowing_input Character string. The sowing window description to resolve.
#' @return A list with fields: type, start, end, shift, source, raw_input, assumptions. If not resolvable, type is "unknown".
#' @examples
#' # Example with AI backend configured:
#' # resolve_sowing("early May")
#' # resolve_sowing("mid June")
#' # resolve_sowing("unknown phrase")
#' @export
resolve_sowing <- function(sowing_input) {
    raw <- tolower(trimws(sowing_input))
    message("Resolving sowing window for input: '", sowing_input, "'")
    res <-  .resolve_sowing_with_ai(raw)
    if (res$type == "unknown") {
        stop("Unable to resolve sowing window from input: ", sowing_input)
    }
    return(res)
}

#' Internal: Use LLM to resolve sowing window
#'
#' @param raw_input Character. The user input.
#' @return A list with structured sowing window fields, or type = "unknown" if not resolvable.
.resolve_sowing_with_ai <- function(raw_input) {
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

- "early <month>" → days 01–10, shift as "mid"
- "mid <month>" → days 11–20, shift as "mid"
- "late <month>" → days 21–end, shift as "mid"

Before a specifc day can be indicated with "before <day> <month>"

-  which should set the shift to "before" respectively
- "end" as the specified day
- "start" as 10 days before the specified day

After a specifc day can be indicated with "after <day> <month>"

- which should set the shift to "after" respectively
- "start" as the specified day
- "end" as 10 days after the specified day




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