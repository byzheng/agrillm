#' Generate a Quarto decision report using AI
#' 
#' This function generates a Quarto (.qmd) report based on the provided decision context and summary of APSIMX outputs. It uses an AI language model to interpret the decision information and produce a structured report that includes narrative explanations and R code for visualisation, following a predefined template.
#' 
#' The generated QMD report will be saved to the specified file path. The function ensures that the AI-generated content adheres to strict constraints, relying solely on the provided decision context and summary data without introducing new assumptions or calculations.
#' 
#' @param apsimx_file Character scalar path to the APSIMX file used in the analysis. This will be referenced in the QMD report.
#' @param decisions List of resolved decision objects containing the decision context, including crop, location
#' metadata, soil conditions, and other relevant information that should be included in the report.
#' @param summary Data frame summarising the APSIMX outputs, which will be available in the QMD environment for generating narrative and code. This should include key metrics and results that the report will discuss.
#' @param qmd_file Character scalar path where the generated QMD file will be saved. The function will write the generated content to this file.
#' @param qmd_template Character scalar path to the QMD template file. If `NULL` (default), a built-in template from the package will be used. The template should contain placeholders for dynamic content and follow the expected structure for the AI to fill in.
#' @param use_ai Logical scalar indicating whether to use AI for generating the QMD content
#' (typically `TRUE`). If `FALSE`, the function will not generate content and will currently throw an error, as non-AI generation is not implemented yet.
#' @param use_cache Logical scalar indicating whether to use cached AI responses if available. This can speed up generation if the same prompt has been used before.
#' @return Character scalar path to the written output QMD file.
#' @export 
generate_decision_qmd <- function(
    apsimx_file,
    decisions,
    summary, 
    qmd_file,
    qmd_template = NULL,
    use_ai = TRUE,
    use_cache = TRUE
) {
    stopifnot(is.list(decisions))
    stopifnot(is.data.frame(summary))
    stopifnot(is.character(qmd_file), length(qmd_file) == 1)
    stopifnot(is.character(apsimx_file), length(apsimx_file) == 1, file.exists(apsimx_file))
    stopifnot(is.logical(use_ai), length(use_ai) == 1)
    stopifnot(is.logical(use_cache), length(use_cache) == 1)
    
    if (is.null(qmd_template)) {
        qmd_template <- system.file("templates", "decision.qmd", package = "agrillm")
    } else {
        stopifnot(is.character(qmd_template), length(qmd_template) == 1, file.exists(qmd_template))
    }

    if (qmd_template == "") {
        stop("QMD template not found in package 'agrillm'. Please ensure the package is installed correctly.")
    }
    if (!use_ai) {
        stop("Non-AI generation not implemented yet. Please set use_ai = TRUE.")
    }

    reply <- .generate_decision_qmd_with_ai(
        apsimx_file,
        decisions,
        summary,
        qmd_template,
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
#' @param summary Data frame summarising the APSIMX outputs, which will be available
#' in the QMD environment for generating narrative and code.
#' @param qmd_template Character scalar path to the QMD template file.
#' @param use_cache Logical scalar indicating whether to use cached AI responses if available.
#' @return Character scalar containing the generated QMD content.
.generate_decision_qmd_with_ai <- function(
    apsimx_file,
    decisions,
    summary,
    qmd_template,
    use_cache = TRUE
) {
    stopifnot(is.list(decisions))
    stopifnot(is.data.frame(summary))
    stopifnot(is.character(qmd_template), length(qmd_template) == 1, file.exists(qmd_template))
    stopifnot(is.character(apsimx_file), length(apsimx_file) == 1, file.exists(apsimx_file))
    stopifnot(is.logical(use_cache), length(use_cache) == 1)

    qmd_template <- readLines(qmd_template, warn = FALSE) |> paste(collapse = "\n")
    decision_str <- .format_list_for_ai(decisions)
    summary_cols <- colnames(summary) |> paste(collapse = ", ")
    recipe_catalogue <- agrillm::recipe_registry() |> .format_list_for_ai()
    prompt <- glue::glue("
You are assisting with the generation of a Quarto (.qmd) decision report.

Your role is to interpret a structured decision context and deterministic analysis outputs,
and to generate clear, auditable narrative and code for reporting purposes.

STRICT CONSTRAINTS:
- Do NOT perform calculations
- Do NOT generate new data
- Do NOT introduce new assumptions
- Do NOT modify analysis logic
- Do NOT invent thresholds or agronomic rules
- Do NOT restructure the template

You MAY:
- summarise and explain existing results
- describe patterns, trends, and trade-offs
- generate Quarto markdown text
- generate R plotting code using ONLY existing objects
- generate inline R code (`r ...`) using ONLY existing objects to reference specific values
- communicate uncertainty and limitations

----------------------
QMD TEMPLATE
----------------------
{qmd_template}

The following keywords are used in the template and should be replaced with actual content:

- {{apsimx_file}} as {apsimx_file}
- {{crop}} as {decisions$crop$crop}

----------------------
DECISION OBJECT (JSON)
----------------------

{decision_str}

Summarise key elements including:
- location
- crop
- soil nitrogen & water
- fertilisation nitrogen
- decision focus

----------------------
AVAILABLE EVIDENCE
----------------------
The following R objects are available in the QMD environment:

- summary: data.frame with {nrow(summary)} rows and columns: {summary_cols}

Use R code (not numbers in text) to access values and generate plots.

----------------------
AVAILABLE RECIPES
----------------------

{recipe_catalogue}

Rules:
- Prefer R code using existing objects
- Return ONLY the complete QMD document
- make sure check chunk separators (```{r} ... ```) are correct, do not use alternative separators (e.g. ````{r} ... ````
")

    response <- agrillm::llm_chat(prompt, use_cache = use_cache)
    reply <- tidyllm::get_reply(response) 
}
