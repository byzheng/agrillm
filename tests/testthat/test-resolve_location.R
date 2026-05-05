test_that("resolve_location returns provided coordinates without AI", {
    result <- resolve_location(
        location_input = "Wagga Wagga, NSW",
        latitude = -35.1082,
        longitude = 147.3598,
        use_ai = FALSE
    )

    expect_equal(result$latitude, -35.1082)
    expect_equal(result$longitude, 147.3598)
    expect_equal(result$display_name, "Wagga Wagga, NSW")
    expect_equal(result$source, "provided_coordinates")
    expect_equal(result$confidence, "high")
    expect_equal(result$raw_input, "Wagga Wagga, NSW")
    expect_null(result$assumptions)
})

test_that("resolve_location errors when coordinates cannot be resolved and AI is disabled", {
    expect_warning(
        resolve_location("Unknown Place", use_ai = FALSE),
        "Unable to resolve location to coordinates"
    )
})

test_that("resolve_location returns unknown when coordinates are optional and AI is disabled", {
    expect_warning(
        result <- resolve_location(
            "Unknown Place",
            use_ai = FALSE
        ),
        "Unable to resolve location to coordinates"
    )

    expect_null(result$latitude)
    expect_null(result$longitude)
    expect_null(result$display_name)
    expect_equal(result$source, "unknown")
    expect_equal(result$confidence, "low")
    expect_equal(result$raw_input, "Unknown Place")
    expect_equal(result$assumptions, "Location unresolved")
})
