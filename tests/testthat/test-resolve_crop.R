test_that("resolve_crop returns direct match for exact or synonym", {
    agrillm_options$reset()
    expect_equal(resolve_crop("wheat", use_ai = FALSE)$crop, "wheat")
    expect_equal(resolve_crop("Winter Wheat", use_ai = FALSE)$crop, "wheat")
    expect_equal(resolve_crop("malting barley", use_ai = FALSE)$crop, "barley")
    expect_equal(resolve_crop("oilseed rape", use_ai = FALSE)$crop, "canola")
})

test_that("resolve_crop uses heuristics if no direct match", {
    agrillm_options$reset()
    expect_equal(resolve_crop("strange wheat", use_ai = FALSE)$crop, "wheat")
    expect_equal(resolve_crop("super barley", use_ai = FALSE)$crop, "barley")
    expect_equal(resolve_crop("double canola", use_ai = FALSE)$crop, "canola")
})

test_that("resolve_crop errors if crop cannot be resolved and use_ai is FALSE", {
    agrillm_options$reset()
    expect_error(resolve_crop("notacrop", use_ai = FALSE))
})

test_that("resolve_crop returns correct crop for direct match", {
    agrillm_options$reset()
    expect_equal(
        resolve_crop("wheat", use_ai = FALSE)$crop,
        "wheat"
    )
    expect_equal(
        resolve_crop("Winter Wheat", use_ai = FALSE)$crop,
        "wheat"
    )
    expect_equal(
        resolve_crop("malting barley", use_ai = FALSE)$crop,
        "barley"
    )
    expect_equal(
        resolve_crop("oilseed rape", use_ai = FALSE)$crop,
        "canola"
    )
})

test_that("resolve_crop uses heuristics if no direct match", {
    agrillm_options$reset()
    expect_equal(
        resolve_crop("strange wheat", use_ai = FALSE)$crop,
        "wheat"
    )
    expect_equal(
        resolve_crop("super barley", use_ai = FALSE)$crop,
        "barley"
    )
    expect_equal(
        resolve_crop("double canola", use_ai = FALSE)$crop,
        "canola"
    )
    expect_equal(
        resolve_crop("rapeseed meal", use_ai = FALSE)$crop,
        "canola"
    )
})

test_that("resolve_crop errors if crop cannot be resolved and use_ai is FALSE", {
    agrillm_options$reset()
    expect_error(resolve_crop("notacrop", use_ai = FALSE),
                "Unable to resolve crop")
})
