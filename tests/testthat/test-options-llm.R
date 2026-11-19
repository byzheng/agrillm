test_that("agrillm_options$get('llm') returns correct defaults", {
    agrillm_options$reset()
    llm <- agrillm_options$get("llm")
    expect_type(llm, "list")
    expect_equal(llm$provider, "azure")
    expect_true("azure" %in% names(llm))
    expect_equal(llm$azure$api_version, "2024-12-01-preview")
    expect_equal(llm$azure$resource_name, "")
    expect_equal(llm$azure$deployment_name, "gpt-5.2")
})

test_that("agrillm_options$set updates llm and get returns new values", {
    agrillm_options$reset()
    agrillm_options$set(llm = list(
        provider = "azure",
        azure = list(
            api_version = "2025-01-01-preview",
            resource_name = "test-resource",
            deployment_name = "gpt-6.0"
        )
    ))
    llm <- agrillm_options$get("llm")
    expect_equal(llm$azure$api_version, "2025-01-01-preview")
    expect_equal(llm$azure$resource_name, "test-resource")
    expect_equal(llm$azure$deployment_name, "gpt-6.0")
})

test_that("agrillm_options$reset restores default llm", {
    agrillm_options$set(llm = list(
        provider = "azure",
        azure = list(
            api_version = "2025-01-01-preview",
            resource_name = "test-resource",
            deployment_name = "gpt-6.0"
        )
    ))
    agrillm_options$reset()
    llm <- agrillm_options$get("llm")
    expect_equal(llm$azure$api_version, "2024-12-01-preview")
    expect_equal(llm$azure$resource_name, "")
    expect_equal(llm$azure$deployment_name, "gpt-5.2")
})

test_that("agrillm_options$set errors for invalid llm value", {
    expect_error(agrillm_options$set(llm = "notalist"))
    expect_error(agrillm_options$set(llm = list(provider = "azure2")))
})
