
resolve_crop <- function(crop_input, use_ai = TRUE) {
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
    stop("AI-based resolution is not implemented yet. Unable to resolve crop: ", crop_input)
    # ai_crop <- resolve_crop_with_ai(raw, names(SUPPORTED_CROPS))
    # return(
    #     list(
    #         crop = ai_crop,
    #         source = "ai_resolution",
    #         raw_input = crop_input
    #         )
    # )

}

.resolve_crop_with_ai <- function(raw_input, candidate_crops) {
    prompt <- sprintf("
You are mapping a farmer free text crop description to a supported crop
for an agricultural simulation system.

Supported crops:
- wheat
- barley
- canola

Farmer input: %s

Task:
Return the single best matching crop from the supported list.
If none clearly match, return \"unknown\".

Rules:
- Do not invent new crops
- Do not explain
- Return only one word from the list or \"unknown\"
    
    ")
}