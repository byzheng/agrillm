test_that("agrillm_options returns all options when called with no arguments", {
    opts <- agrillm_options()
    expect_type(opts, "list")
    expect_true("openai" %in% names(opts))
    expect_true("default" %in% names(opts))
})

test_that("agrillm_options sets and retrieves openai.apikey", {
    # Reset to ensure clean state
    agrillm_reset()

    # Set API key
    agrillm_options(openai.apikey = "test_key_123")

    # Verify it was set
    key <- get_agrillm_option("openai.apikey")
    expect_equal(key, "test_key_123")

    # Clean up
    agrillm_reset()
})

test_that("agrillm_options sets and retrieves default option", {
    # Reset to ensure clean state
    agrillm_reset()

    # Set default
    agrillm_options(default = "openai")

    # Verify it was set
    default <- get_agrillm_option("default")
    expect_equal(default, "openai")
    # Clean up
    agrillm_reset()
})

test_that("agrillm_options throws error for unnamed arguments", {
    expect_error(
        agrillm_options("value"),
        "All arguments must be named"
    )
})

test_that("agrillm_options throws error for invalid option name format", {
    expect_error(
        agrillm_options(invalid.option.name = "value"),
        "Invalid option name"
    )
})

test_that("agrillm_options throws error for unknown group", {
    expect_error(
        agrillm_options(unknown.option = "value"),
        "Unknown group.*must be 'openai'"
    )
})

test_that("agrillm_options throws error for unknown top-level option", {
    expect_error(
        agrillm_options(unknown = "value"),
        "Unknown top-level option.*must be 'default'"
    )
})

test_that("agrillm_reset restores default values", {
    # Set a custom value
    agrillm_options(openai.apikey = "custom_key")
    expect_equal(get_agrillm_option("openai.apikey"), "custom_key")

    # Reset
    agrillm_reset()

    # Verify reset to default
    key <- get_agrillm_option("openai.apikey")
    expect_equal(key, "")
})

test_that("get_agrillm_option retrieves nested options", {
    agrillm_reset()
    agrillm_options(openai.apikey = "test_key")

    value <- get_agrillm_option("openai.apikey")
    expect_equal(value, "test_key")

    agrillm_reset()
})

test_that("get_agrillm_option retrieves top-level options", {
    agrillm_reset()

    value <- get_agrillm_option("default")
    expect_equal(value, "openai")

    agrillm_reset()
})

test_that(".check_option_value enforces type checking", {
    # Test that wrong type throws error
    expect_error(
        .check_option_value("string", 123),
        "Option expects character, got numeric"
    )
})

test_that(".check_option_value rejects NULL values", {
    expect_error(
        .check_option_value("string", NULL),
        "Option value cannot be NULL"
    )
})

test_that(".check_option_value enforces length checking", {
    expect_error(
        .check_option_value("single", c("multiple", "values")),
        "Option expects length 1, got length 2"
    )
})

test_that("agrillm_options returns invisibly when setting options", {
    result <- withVisible(agrillm_options(openai.apikey = "test"))
    expect_false(result$visible)
    expect_type(result$value, "list")

    agrillm_reset()
})

test_that("agrillm_reset returns invisibly", {
    result <- withVisible(agrillm_reset())
    expect_false(result$visible)
    expect_type(result$value, "list")
})
