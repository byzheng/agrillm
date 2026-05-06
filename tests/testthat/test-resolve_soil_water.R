test_that("resolve_soil_water returns unresolvable when AI is disabled", {
    expect_warning(
        result <- resolve_soil_water("some unclear description", use_ai = FALSE),
        "Unable to resolve soil water status from input"
    )

    expect_equal(result$type, "unknown")
    expect_null(result$pawc_mm)
    expect_null(result$fraction)
    expect_null(result$status)
    expect_equal(result$source, "unresolvable")
    expect_equal(result$raw_input, "some unclear description")
    expect_equal(
        result$assumptions,
        "LLM fallback disabled and input could not be resolved with deterministic rules"
    )
})

test_that("resolve_soil_water uses AI fallback when needed", {
    with_mocked_bindings(
        .resolve_soil_water_with_ai = function(raw_input, use_cache = TRUE) {
            list(
                type = "categorical",
                pawc_mm = NULL,
                fraction = 0.5,
                status = "half",
                source = "ai_resolution",
                raw_input = raw_input,
                assumptions = NULL,
                use_cache = use_cache
            )
        },
        {
            result <- resolve_soil_water("Profile about half full", use_cache = FALSE)

            expect_equal(result$type, "categorical")
            expect_null(result$pawc_mm)
            expect_equal(result$fraction, 0.5)
            expect_equal(result$status, "half")
            expect_equal(result$source, "ai_resolution")
            expect_equal(result$raw_input, "profile about half full")
            expect_false(result$use_cache)
        }
    )
})

test_that("resolve_soil_water returns unknown when AI cannot resolve input", {
    with_mocked_bindings(
        .resolve_soil_water_with_ai = function(raw_input, use_cache = TRUE) {
            list(
                type = "unknown",
                pawc_mm = NULL,
                fraction = NULL,
                status = "unknown",
                source = "ai_resolution",
                raw_input = raw_input,
                assumptions = NULL
            )
        },
        {
            expect_warning(
                result <- resolve_soil_water("Not sure"),
                "Unable to resolve soil water status from input"
            )

            expect_equal(result$type, "unknown")
            expect_null(result$pawc_mm)
            expect_null(result$fraction)
            expect_null(result$status)
            expect_equal(result$source, "ai_resolution")
            expect_equal(result$raw_input, "Not sure")
            expect_equal(
                result$assumptions,
                "LLM was unable to interpret the input as a soil water status"
            )
        }
    )
})
