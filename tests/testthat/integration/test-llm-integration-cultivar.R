test_that("resolve_cultivar returns a cultivar for fuzzy cultivar input [LLM integration]", {
    skip_if_no_llm()

    cultivar_db <- data.frame(
        name = c("Scepter", "Mace"),
        stringsAsFactors = FALSE
    )

    result <- resolve_cultivar(
        "I think it was scepter wheat",
        "wheat",
        cultivar_db = cultivar_db
    )

    expect_equal(result$cultivar, "scepter")
    expect_equal(result$source, "ai_resolution")
    expect_equal(result$raw_input, "i think it was scepter wheat")
})
