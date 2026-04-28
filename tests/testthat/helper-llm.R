
# Helper to skip LLM integration tests if not enabled or credentials missing
skip_if_no_llm <- function() {
    skip_on_cran()
    if (Sys.getenv("RUN_LLM_TESTS") != "TRUE") skip("LLM tests are disabled by default")
    if (Sys.getenv("AZURE_OPENAI_API_KEY") == "" || Sys.getenv("AZURE_ENDPOINT_URL") == "") {
        skip("Azure credentials not set")
    }
}
