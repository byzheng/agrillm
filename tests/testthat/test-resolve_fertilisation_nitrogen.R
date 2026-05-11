test_that("resolve_fertilisation_nitrogen parses numeric nitrogen values", {
    result <- resolve_fertilisation_nitrogen("50 kg/ha", use_ai = FALSE)

    expect_equal(result$type, "measured")
    expect_equal(result$value, 50)
    expect_equal(result$unit, "kg/ha")
    expect_equal(result$source, "user_provided")
    expect_equal(result$raw_input, "50 kg/ha")
})

test_that("resolve_fertilisation_nitrogen resolves categorical aliases directly", {
    expect_equal(resolve_fertilisation_nitrogen("low n", use_ai = FALSE)$status, "low")
    expect_equal(resolve_fertilisation_nitrogen("background n only", use_ai = FALSE)$status, "moderate")
    expect_equal(resolve_fertilisation_nitrogen("plenty of nitrogen", use_ai = FALSE)$status, "high")
})

test_that("resolve_fertilisation_nitrogen returns unknown when AI is disabled", {
    expect_warning(
        result <- resolve_fertilisation_nitrogen("some unclear description", use_ai = FALSE),
        "Unable to resolve fertilisation nitrogen input"
    )

    expect_equal(result$type, "unknown")
    expect_equal(result$source, "unresolvable")
    expect_equal(result$raw_input, "some unclear description")
    expect_equal(
        result$assumptions,
        "LLM fallback disabled and input could not be resolved with deterministic rules"
    )
})

test_that("resolve_fertilisation_nitrogen uses AI fallback when needed", {
    with_mocked_bindings(
        .resolve_fertilisation_n_with_ai = function(raw_input, use_cache = TRUE) {
            list(
                type = "categorical",
                status = "moderate",
                source = "ai_resolution",
                raw_input = raw_input,
                assumptions = NULL,
                use_cache = use_cache
            )
        },
        {
            result <- resolve_fertilisation_nitrogen("Patchy but probably okay", use_cache = FALSE)

            expect_equal(result$type, "categorical")
            expect_equal(result$status, "moderate")
            expect_equal(result$source, "ai_resolution")
            expect_equal(result$raw_input, "patchy but probably okay")
            expect_false(result$use_cache)
        }
    )
})