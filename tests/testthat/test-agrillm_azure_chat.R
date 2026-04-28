

test_that("agrillm_azure_chat calls wrapper with correct args", {
    with_mocked_bindings(
        .azure_chat = function(message, deployment, api_version, ...) {
            list(
                message = message,
                deployment = deployment,
                api_version = api_version
            )
        },
        {
            agrillm_options$reset()
            result <- azure_chat("test prompt")

            expect_equal(result$message, "test prompt")
            expect_equal(result$deployment, agrillm_options$get("llm")$azure$deployment_name)
            expect_equal(result$api_version, agrillm_options$get("llm")$azure$api_version)
        }
    )
})
