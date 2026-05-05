
#' Resolve a crop name to a supported crop
#'
#' This function attempts to map a user-provided crop name or synonym to a supported crop name
#' using exact/synonym matching, heuristics, and optionally an LLM (AI) fallback.
#'
#' @param crop_input Character string. The crop name or description to resolve.
#' @param use_ai Logical. Whether to use AI fallback if no match is found. Default: TRUE.
#' @param use_cache Logical. Whether to use caching for AI responses. Default: TRUE.
#' @return A list with elements: crop (resolved crop name), source ("direct_match", "heuristic", or "ai_resolution"), raw_input (original input).
#' @examples
#' resolve_crop("wheat")
#' resolve_crop("Winter Wheat")
#' # resolve_crop("unknown crop", use_ai = FALSE) # should error
#' 
#' # With AI fallback (if configured)
#' # resolve_crop("unknown crop", use_ai = TRUE)
#' 
#' @export
resolve_crop <- function(crop_input, use_ai = TRUE, use_cache = TRUE) {
    stopifnot(is.character(crop_input), length(crop_input) == 1)
    raw <- tolower(trimws(crop_input))

    SUPPORTED_CROPS <- agrillm_options$get("crops")

    # 1. Exact / synonym match
    for (crop in names(SUPPORTED_CROPS)) {
        if (raw %in% tolower(SUPPORTED_CROPS[[crop]])) {
            return(list(
                crop = crop,
                source = "direct_match",
                raw_input = crop_input
            ))
        }
    }

    # 2. Simple heuristic (still deterministic)
    raw_crops <- names(SUPPORTED_CROPS)
    for (crop in raw_crops) {
        for (syn in tolower(SUPPORTED_CROPS[[crop]])) {
            if (grepl(syn, raw)) {
                return(list(crop = crop, source = "heuristic", raw_input = crop_input))
            }
        }
    }

    # 3. AI fallback (optional)
    if (!use_ai) {
        stop("Unable to resolve crop: ", crop_input)
    }
    message("No direct match for crop '", crop_input, "'. Using AI to attempt resolution...")
    ai_crop <- .resolve_crop_with_ai(raw, names(SUPPORTED_CROPS), use_cache = use_cache)
    return(
        list(
            crop = ai_crop,
            source = "ai_resolution",
            raw_input = crop_input
        )
    )

}

#' Internal: Use LLM to resolve crop
#'
#' @param raw_input Character. The user input.
#' @param candidate_crops Character vector. Supported crops.
#' @param use_cache Logical. Whether to use caching for AI responses.
#' @return The best matching crop or "unknown".
.resolve_crop_with_ai <- function(raw_input, candidate_crops, use_cache = TRUE) {
    stopifnot(is.character(raw_input), length(raw_input) == 1)
    stopifnot(is.character(candidate_crops), length(candidate_crops) > 0)
    
    prompt <- sprintf("
You are mapping a farmer free text crop description to a supported crop
for an agricultural simulation system.

Supported crops: %s

Farmer input: %s

Task:
Return the single best matching crop from the supported list.
If none clearly match, return \"unknown\".

Rules:
- Do not invent new crops
- Do not explain
- Return only one word from the list or \"unknown\"
    
", 
    paste(candidate_crops, collapse = ","),
    raw_input
    )
    response <- llm_chat(prompt, use_cache = use_cache)
    reply <- tidyllm::get_reply(response)  
    reply <- tolower(trimws(reply))
    if (reply %in% candidate_crops) {
        return(reply)
    }
    warning("LLM was unable to resolve crop input: ", raw_input)
    return(
        list(
            crop = "unknown",
            source = "ai_resolution",
            raw_input = raw_input,
            assumptions = "LLM was unable to match input to supported crops"
        )
    )
}
