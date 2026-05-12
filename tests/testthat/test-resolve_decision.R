test_that("resolve_decision returns unknown when AI is disabled", {
    expect_warning(
        result <- resolve_decision("some unclear decision", use_ai = FALSE),
        "Unable to resolve decision from input"
    )

    expect_equal(result$type, "unknown")
    expect_null(result$year_start)
    expect_null(result$year_end)
    expect_equal(result$source, "unresolvable")
    expect_equal(result$raw_input, "some unclear decision")
    expect_equal(
        result$assumptions,
        "LLM fallback disabled and input could not be resolved with deterministic rules"
    )
})

test_that("resolve_decision uses AI fallback when needed", {
    with_mocked_bindings(
        .resolve_decision_with_ai = function(raw_input, use_cache = TRUE) {
            list(
                type = "cultivar_choice",
                year_start = 2000L,
                year_end = 2020L,
                source = "ai_resolution",
                raw_input = raw_input,
                assumptions = NULL,
                use_cache = use_cache
            )
        },
        {
            result <- resolve_decision("Cultivar choice for the last 20 years", use_cache = FALSE)

            expect_equal(result$type, "cultivar_choice")
            expect_equal(result$year_start, 2000L)
            expect_equal(result$year_end, 2020L)
            expect_equal(result$source, "ai_resolution")
            expect_equal(result$raw_input, "cultivar choice for the last 20 years")
            expect_false(result$use_cache)
        }
    )
})

test_that("resolve_decision returns unknown when AI cannot interpret input", {
    with_mocked_bindings(
        .resolve_decision_with_ai = function(raw_input, use_cache = TRUE) {
            list(
                type = "unknown",
                year_start = NULL,
                year_end = NULL,
                source = "ai_resolution",
                raw_input = raw_input,
                assumptions = NULL
            )
        },
        {
            expect_warning(
                result <- resolve_decision("Not sure what decision this is"),
                "Unable to resolve decision from input"
            )

            expect_equal(result$type, "unknown")
            expect_null(result$year_start)
            expect_null(result$year_end)
            expect_equal(result$source, "ai_resolution")
            expect_equal(result$raw_input, "not sure what decision this is")
            expect_equal(
                result$assumptions,
                "LLM was unable to interpret the input as a known decision"
            )
        }
    )
})