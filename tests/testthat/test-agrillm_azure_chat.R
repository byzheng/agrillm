

test_that("agrillm_azure_chat calls wrapper with correct args", {
    llm_cache_clear()

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

    llm_cache_clear()
})

test_that("azure_chat reuses cached responses for identical requests", {
    llm_cache_clear()
    call_count <- 0

    with_mocked_bindings(
        .azure_chat = function(message, deployment, api_version, ...) {
            call_count <<- call_count + 1
            list(
                message = message,
                deployment = deployment,
                api_version = api_version,
                call_count = call_count
            )
        },
        {
            agrillm_options$reset()

            first <- azure_chat("cached prompt", use_cache = TRUE)
            second <- azure_chat("cached prompt", use_cache = TRUE)

            expect_equal(call_count, 1)
            expect_equal(first, second)
            expect_equal(llm_cache_size(provider = "azure"), 1)
        }
    )

    llm_cache_clear()
})

test_that("llm_cache_clear removes cached azure entries", {
    llm_cache_clear()

    with_mocked_bindings(
        .azure_chat = function(message, deployment, api_version, ...) {
            list(message = message)
        },
        {
            agrillm_options$reset()

            azure_chat("first prompt", use_cache = TRUE)
            azure_chat("second prompt", use_cache = TRUE)

            expect_equal(llm_cache_size(provider = "azure"), 2)

            removed <- llm_cache_clear(message = "first prompt", provider = "azure")
            cache_info <- llm_cache_info(provider = "azure")

            expect_equal(removed, 1)
            expect_equal(llm_cache_size(provider = "azure"), 1)
            expect_equal(cache_info$provider, "azure")
            expect_equal(cache_info$message, "second prompt")
        }
    )

    llm_cache_clear()
})

test_that("azure_chat reloads persisted cache from disk", {
    llm_cache_clear()
    cache_path <- tempfile(fileext = ".rds")
    call_count <- 0

    with_mocked_bindings(
        .azure_chat = function(message, deployment, api_version, ...) {
            call_count <<- call_count + 1
            list(
                message = message,
                deployment = deployment,
                api_version = api_version,
                call_count = call_count
            )
        },
        {
            agrillm_options$reset()

            first <- azure_chat(
                "persisted prompt",
                use_cache = TRUE,
                persist_cache = TRUE,
                cache_path = cache_path
            )

            llm_cache_clear()

            second <- azure_chat(
                "persisted prompt",
                use_cache = TRUE,
                persist_cache = TRUE,
                cache_path = cache_path
            )

            expect_equal(call_count, 1)
            expect_equal(first, second)
            expect_true(file.exists(cache_path))
        }
    )

    llm_cache_clear()
    llm_cache_delete(cache_path)
})

test_that("llm_cache_save and llm_cache_load round-trip entries", {
    llm_cache_clear()
    cache_path <- tempfile(fileext = ".rds")

    with_mocked_bindings(
        .azure_chat = function(message, deployment, api_version, ...) {
            list(message = message, deployment = deployment)
        },
        {
            agrillm_options$reset()

            azure_chat("save prompt", use_cache = TRUE)
            llm_cache_save(cache_path)
            llm_cache_clear()

            loaded <- llm_cache_load(cache_path)
            cache_info <- llm_cache_info(provider = "azure")

            expect_equal(loaded, 1)
            expect_equal(llm_cache_size(provider = "azure"), 1)
            expect_equal(cache_info$message, "save prompt")
        }
    )

    llm_cache_clear()
    llm_cache_delete(cache_path)
})

test_that("llm_cache_path creates the default cache directory", {
    base_dir <- tempfile(pattern = "agrillm-cache-")
    dir.create(base_dir)

    old_wd <- getwd()
    setwd(base_dir)
    on.exit({
        setwd(old_wd)
        unlink(base_dir, recursive = TRUE, force = TRUE)
    }, add = TRUE)

    cache_path <- llm_cache_path()

    expect_true(dir.exists(file.path(base_dir, ".cache_agrillm")))
    expect_equal(
        normalizePath(cache_path, winslash = "/", mustWork = FALSE),
        normalizePath(
            file.path(base_dir, ".cache_agrillm", "cache.rds"),
            winslash = "/",
            mustWork = FALSE
        )
    )
})

test_that("llm_cache_clear removes the default cache directory", {
    base_dir <- tempfile(pattern = "agrillm-cache-")
    dir.create(base_dir)

    old_wd <- getwd()
    setwd(base_dir)
    on.exit({
        setwd(old_wd)
        unlink(base_dir, recursive = TRUE, force = TRUE)
    }, add = TRUE)

    cache_path <- llm_cache_path()
    saveRDS(list(version = 1L, saved_at = Sys.time(), entries = list()), cache_path)

    expect_true(dir.exists(file.path(base_dir, ".cache_agrillm")))

    llm_cache_clear()

    expect_false(dir.exists(file.path(base_dir, ".cache_agrillm")))
})
