library(testthat)
library(tidyllm)

test_that("llm_chat calls agrillm_azure_chat for azure provider", {
    # Mock azure_chat
    with_mocked_bindings(
        azure_chat = function(prompt, use_cache = FALSE, ...) {
            list(
                prompt = prompt,
                use_cache = use_cache,
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

test_that("llm_chat forwards use_cache to azure_chat", {
    with_mocked_bindings(
        azure_chat = function(prompt, use_cache = FALSE, persist_cache = FALSE, cache_path = NULL, ...) {
            list(
                prompt = prompt,
                use_cache = use_cache,
                persist_cache = persist_cache,
                cache_path = cache_path
            )
        },
        {
            agrillm_options$reset()
            result <- llm_chat("test prompt", use_cache = TRUE)

            expect_equal(result$prompt, "test prompt")
            expect_true(result$use_cache)
        }
    )
})

test_that("llm_chat forwards persistent cache arguments to azure_chat", {
    with_mocked_bindings(
        azure_chat = function(prompt, use_cache = FALSE, persist_cache = FALSE, cache_path = NULL, ...) {
            list(
                prompt = prompt,
                use_cache = use_cache,
                persist_cache = persist_cache,
                cache_path = cache_path
            )
        },
        {
            agrillm_options$reset()
            cache_path <- tempfile(fileext = ".rds")
            result <- llm_chat(
                "test prompt",
                use_cache = TRUE,
                persist_cache = TRUE,
                cache_path = cache_path
            )

            expect_equal(result$prompt, "test prompt")
            expect_true(result$use_cache)
            expect_true(result$persist_cache)
            expect_equal(result$cache_path, cache_path)
        }
    )
})
