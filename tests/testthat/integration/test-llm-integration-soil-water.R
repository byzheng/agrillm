test_that("resolve_soil_water returns a structured status for soil water input [LLM integration]", {
    skip_if_no_llm()

    result <- resolve_soil_water("PAWC about 100mm, full profile")

    expect_true(result$type %in% c("measured", "fraction", "categorical", "unknown"))
    expect_true(is.null(result$pawc_mm) || is.numeric(result$pawc_mm))
    expect_true(is.null(result$fraction) || (is.numeric(result$fraction) && result$fraction >= 0 && result$fraction <= 1))
    expect_true(is.null(result$status) || result$status %in% c("full", "half", "low", "unknown"))
    expect_true(all(c("type", "pawc_mm", "fraction", "status") %in% names(result)))
})
