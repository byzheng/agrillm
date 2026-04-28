test_that("agrillm_options() returns all options when called with no arguments", {
    # Reset to ensure clean state
    agrillm_options$reset()

    options <- agrillm_options$get()
    expect_type(options, "list")    
    expect_true("crops" %in% names(options))
})
