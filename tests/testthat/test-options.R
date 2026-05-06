test_that("agrillm_options() returns all options when called with no arguments", {
    # Reset to ensure clean state
    agrillm_options$reset()

    options <- agrillm_options$get()
    expect_type(options, "list")    
    expect_true("cache_dir" %in% names(options))
    expect_true("crops" %in% names(options))
})

test_that("agrillm_options$get('cache_dir') returns the default cache directory", {
    agrillm_options$reset()

    cache_dir <- agrillm_options$get("cache_dir")

    expect_type(cache_dir, "character")
    expect_equal(cache_dir, ".agrillm")
})

test_that("agrillm_options$set updates cache_dir and reset restores the default", {
    agrillm_options$reset()
    agrillm_options$set(cache_dir = "custom-cache")

    expect_equal(agrillm_options$get("cache_dir"), "custom-cache")

    agrillm_options$reset()

    expect_equal(agrillm_options$get("cache_dir"), ".agrillm")
})

test_that("agrillm_options$set errors for invalid cache_dir value", {
    expect_error(agrillm_options$set(cache_dir = c("a", "b")))
    expect_error(agrillm_options$set(cache_dir = 1))
})
