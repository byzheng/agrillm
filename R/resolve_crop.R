
#' Resolve a crop name to a supported crop
#'
#' This function attempts to map a user-provided crop name or synonym to a supported crop name
#' using exact/synonym matching, heuristics, and optionally an LLM (AI) fallback.
#' 
#' @details 
#' 
#' The function first checks for exact matches against a predefined list of supported crops and their synonyms. If no match is found, it applies simple heuristics (e.g., substring matching). If still unresolved and `use_ai` is TRUE, it constructs a prompt to an LLM to attempt resolution. The output includes the resolved crop name, the source of the resolution, the original input, and any assumptions made.
#' 
#' The output is a list with the following fields:
#' 
#' list(
#'    crop = "wheat" | "known" | "categorical" | "unknown",
#'    source = "direct_match" | "heuristic" | "ai_resolution" | "unresolvable",
#'    raw_input = "wheat",
#'    assumptions = NULL
#' )
#'
#' @param crop_input Character string. The crop name or description to resolve.
#' @param use_ai Logical. Whether to use AI fallback if no match is found. Default: TRUE.
#' @param use_cache Logical. Whether to use caching for AI responses. Default: TRUE.
#' @return A list with elements: crop (resolved crop name), source ("direct_match", "heuristic", or "ai_resolution"), raw_input (original input), assumption (description of how the resolution was made).
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
    stopifnot(is.logical(use_ai), length(use_ai) == 1)
    stopifnot(is.logical(use_cache), length(use_cache) == 1)
    raw <- tolower(trimws(crop_input))

    SUPPORTED_CROPS <- agrillm_options$get("crops")

    # 1. Exact / synonym match
    for (crop in names(SUPPORTED_CROPS)) {
        if (raw %in% tolower(SUPPORTED_CROPS[[crop]])) {
            return(list(
                crop = crop,
                source = "direct_match",
                raw_input = crop_input,
                assumption = paste0("matched against synonyms: ", paste(SUPPORTED_CROPS[[crop]], collapse = ", "))
            ))
        }
    }

    # 2. Simple heuristic (still deterministic)
    raw_crops <- names(SUPPORTED_CROPS)
    for (crop in raw_crops) {
        for (syn in tolower(SUPPORTED_CROPS[[crop]])) {
            if (grepl(syn, raw)) {
                return(list(
                    crop = crop,
                    source = "heuristic",
                    raw_input = crop_input,
                    assumption = paste0("matched against synonyms: ", paste(SUPPORTED_CROPS[[crop]], collapse = ", "))
                ))
            }
        }
    }

    # 3. AI fallback (optional)
    if (!use_ai) {
        warning("Unable to resolve crop: ", crop_input)
        return(list(
            crop = "unknown",
            source = "unresolvable",
            raw_input = crop_input,
            assumption = "no match found and AI fallback disabled"
        )) 
    }
    message("No direct match for crop '", crop_input, "'. Using AI to attempt resolution...")
    ai_crop <- .resolve_crop_with_ai(raw, names(SUPPORTED_CROPS))
    return(
        list(
            crop = ai_crop,
            source = "ai_resolution",
            raw_input = crop_input,
            assumption = "resolved using AI fallback"
        )
    )

}

#' Internal: Use LLM to resolve crop
#'
#' @param raw_input Character. The user input.
#' @param candidate_crops Character vector. Supported crops.
#' @return The best matching crop or "unknown".
.resolve_crop_with_ai <- function(raw_input, candidate_crops) {
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
    response <- llm_chat(prompt)
    reply <- tidyllm::get_reply(response)  
    reply <- tolower(trimws(reply))
    if (reply %in% candidate_crops) {
        return(reply)
    } else {
        return("unknown")
    }
}
