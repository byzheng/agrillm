library(testthat)
library(tidyllm)

test_that("llm_chat calls agrillm_azure_chat for azure provider", {
    # Mock azure_chat
    with_mocked_bindings(
        azure_chat = function(prompt, ...) {
            list(
                prompt = prompt,
                called = TRUE
            )
        },
        {
            agrillm_options$reset()
            result <- llm_chat("test prompt")
            expect_true(result$called)
            expect_equal(result$prompt, "test prompt")
        }
    )
})
