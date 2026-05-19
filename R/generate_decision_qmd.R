#' Generate a Quarto decision report using AI
#' 
#' This function generates a Quarto (.qmd) report based on the provided decision context and summary of APSIMX outputs. It uses an AI language model to interpret the decision information and produce a structured report that includes narrative explanations and R code for visualisation, following a predefined template.
#' 
#' The generated QMD report will be saved to the specified file path. The function ensures that the AI-generated content adheres to strict constraints, relying solely on the provided decision context and summary data without introducing new assumptions or calculations.
#' 
#' @param apsimx_file Character scalar path to the APSIMX file used in the analysis. This will be referenced in the QMD report.
#' @param decisions List of resolved decision objects containing the decision context, including crop, location
#' metadata, soil conditions, and other relevant information that should be included in the report.
#' @param qmd_file Character scalar path where the generated QMD file will be saved. The function will write the generated content to this file.
#' @param use_ai Logical scalar indicating whether to use AI for generating the QMD content
#' (typically `TRUE`). If `FALSE`, the function will not generate content and will currently throw an error, as non-AI generation is not implemented yet.
#' @param use_cache Logical scalar indicating whether to use cached AI responses if available. This can speed up generation if the same prompt has been used before.
#' @return Character scalar path to the written output QMD file.
#' @export 
generate_decision_qmd <- function(
    apsimx_file,
    decisions,
    qmd_file,
    use_ai = TRUE,
    use_cache = TRUE
) {
    stopifnot(is.list(decisions))
    stopifnot(is.character(qmd_file), length(qmd_file) == 1)
    stopifnot(is.character(apsimx_file), length(apsimx_file) == 1, file.exists(apsimx_file))
    stopifnot(is.logical(use_ai), length(use_ai) == 1)
    stopifnot(is.logical(use_cache), length(use_cache) == 1)
    
    if (!use_ai) {
        stop("Non-AI generation not implemented yet. Please set use_ai = TRUE.")
    }
    context <- list(
        meta = list(
            source = apsimx_file,
            report = "HarvestReport",
            title = "Cultivar Suitability Report",
            author = "Bangyou Zheng",
            date = Sys.Date(),
            notes = c("This is a cultivar suitability report generated using APSIM NG and AI.")
        ),
        vars = list(
            cultivar_col = "Cultivar",
            year_col = "Year",
            sowing_col = "SowingDate",
            yield_col = "Wheat.FrostHeatDamageFunctions.FrostHeatYield",
            years = seq(1996, 2025)
        )
    )
    
    criteria <- list(failure = list(yield_threshold = 1.0))
    options <- list()

    apsim_output <- rapsimng.decide::read_output(
        apsimx_file, 
        "HarvestReport",
        years = context$vars$years
    )

    qmd_doc <- rapsimng.decide::document(apsim_output, "cultivar", context, criteria, options, simplify = TRUE)

    reply <- .generate_decision_qmd_with_ai(
        apsimx_file,
        decisions,
        qmd_doc,
        use_cache = use_cache
    )
    writeLines(reply, con = qmd_file)
    invisible(qmd_file)
}


#' Internal function to generate QMD content using AI
#' 
#' This function constructs a detailed prompt for the AI model, providing the decision context, available data, and recipe catalogue. It then calls the language model to generate the QMD content based on the provided information.
#' 
#' @param apsimx_file Character scalar path to the APSIMX file used in the analysis.
#' @param decisions List of resolved decision objects containing the decision context.
#' @param qmd_doc Character scalar containing the QMD document template.
#' @param use_cache Logical scalar indicating whether to use cached AI responses if available.
#' @return Character scalar containing the generated QMD content.
.generate_decision_qmd_with_ai <- function(
    apsimx_file,
    decisions,
    qmd_doc,
    use_cache = TRUE
) {
    stopifnot(is.list(decisions))
    stopifnot(is.character(apsimx_file), length(apsimx_file) == 1, file.exists(apsimx_file))
    stopifnot(is.logical(use_cache), length(use_cache) == 1)
    stopifnot(is.character(qmd_doc), length(qmd_doc) >= 1)
    
    qmd_doc <- paste(qmd_doc, collapse = "\n")

    decision_str <- .format_list_for_ai(decisions)
    prompt <- glue::glue("
You are assisting with the generation of a Quarto (.qmd) decision report.

Your role is to interpret a structured decision context and deterministic analysis outputs,
and to generate clear, auditable narrative and code for reporting purposes.

STRICT CONSTRAINTS:

- Do NOT perform calculations
- Do NOT generate new data
- Do NOT introduce new assumptions
- Do NOT modify analysis logic
- Do NOT restructure the document
- Do NOT modify R code chunks
- Do NOT make or imply decisions or recommendations
- Do NOT invent thresholds or agronomic rules


You MAY:

- describe patterns visible in tables and figures
- summarise key observations
- communicate variability and risk in neutral terms


TASK:
- Return all contents of original QMD document with placeholders replaced by narrative text
- Replace each HTML comment placeholder <!-- TEXT_xxx --> with a concise paragraph (1-3 sentences)
- Use the text in the html comment as a guide for the content of each paragraph
- Each paragraph must be based ONLY on the visible content in the same section
- Do NOT use information outside that section
- Each replacement must be independent

STYLE:
- Use clear, concise language
- Avoid generic phrases
- Refer to values or trends when visible


----------------------
QMD DOC TEMPLATE

The following QMD document is already fully structured and contains:
- deterministic text
- tables
- figures
- HTML comment placeholders for narrative injection

Your task is to replace ONLY the placeholders with narrative text.

----------------------
{qmd_doc}

")

    response <- llm_chat(prompt, use_cache = use_cache)
    reply <- tidyllm::get_reply(response) 
}
