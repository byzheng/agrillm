test_that("resolve_decision returns a structured decision for a free-text intent [LLM integration]", {
    skip_if_no_llm()

    result <- resolve_decision("I want to look at cultivar choice over the last 20 years")

    expect_true(result$type %in% c(
        "sowing_time_choice",
        "cultivar_choice",
        "yield_risk_assessment",
        "fertilisation_nitrogen_choice",
        "unknown"
    ))
    expect_equal(result$source, "ai_resolution")
    expect_equal(result$raw_input, "i want to look at cultivar choice over the last 20 years")
})