test_that("resolve_soil_nitrogen returns a categorical status for ambiguous soil N input [LLM integration]", {
    skip_if_no_llm()

    result <- resolve_soil_nitrogen("Patchy but probably okay")

    expect_equal(result$type, "categorical")
    expect_true(result$status %in% c("low", "moderate", "high", "unknown"))
    expect_equal(result$source, "ai_resolution")
    expect_equal(result$raw_input, "patchy but probably okay")
})