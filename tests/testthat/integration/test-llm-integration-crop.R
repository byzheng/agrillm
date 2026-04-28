
test_that("resolve_crop returns a crop for 'wheat' [LLM integration]", {
    skip_if_no_llm()
    result <- resolve_crop("wheat")
    expect_equal(result$crop, "wheat")
    expect_equal(result$source, "direct_match")
    expect_equal(result$raw_input, "wheat")
})


test_that("resolve_crop returns a crop for 'I will sow wheat' [LLM integration]", {
    skip_if_no_llm()
    result <- resolve_crop("I will sow wheat")
    expect_equal(result$crop, "wheat")
    expect_equal(result$source, "heuristic")
    expect_equal(result$raw_input, "I will sow wheat")
})


test_that("resolve_crop returns a crop for 'whaet' [LLM integration]", {
    skip_if_no_llm()
    result <- resolve_crop("whaet")
    expect_equal(result$crop, "wheat")
    expect_equal(result$source, "ai_resolution")
    expect_equal(result$raw_input, "whaet")
})


test_that("resolve_crop returns a crop for 'notknowcrop' [LLM integration]", {
    skip_if_no_llm()
    result <- resolve_crop("notknowcrop")
    expect_equal(result$crop, "unknown")
    expect_equal(result$source, "ai_resolution")
    expect_equal(result$raw_input, "notknowcrop")
})


