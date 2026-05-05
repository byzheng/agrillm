test_that("resolve_location returns coordinates for 'Wagga Wagga, NSW' [LLM integration]", {
    skip_if_no_llm()

    result <- resolve_location("Wagga Wagga, NSW")

    expect_false(is.null(result$latitude))
    expect_false(is.null(result$longitude))
    expect_type(result$latitude, "double")
    expect_type(result$longitude, "double")
    expect_equal(result$display_name, "Wagga Wagga, NSW")
    expect_equal(result$source, "ai_resolution")
    expect_equal(result$raw_input, "Wagga Wagga, NSW")
})