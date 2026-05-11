test_that("resolve_fertilisation_nitrogen returns a categorical status for ambiguous fertilisation N input [LLM integration]", {
    skip_if_no_llm()

    result <- resolve_fertilisation_nitrogen("Patchy but probably okay")

    expect_equal(result$type, "categorical")
    expect_true(result$status %in% c("low", "moderate", "high", "unknown"))
    expect_equal(result$source, "ai_resolution")
    expect_equal(result$raw_input, "patchy but probably okay")
})