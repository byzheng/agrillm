test_that("resolve_cultivar returns direct match for supported cultivar", {
    cultivar_db <- data.frame(name = c("Scepter", "Mace"), stringsAsFactors = FALSE)

    result <- resolve_cultivar("Scepter", "wheat", cultivar_db = cultivar_db, use_ai = FALSE)

    expect_equal(result$cultivar, "scepter")
    expect_equal(result$source, "direct_match")
    expect_equal(result$raw_input, "Scepter")
    expect_null(result$assumptions)
})

test_that("resolve_cultivar returns unresolvable when AI is disabled", {
    cultivar_db <- data.frame(name = c("Scepter", "Mace"), stringsAsFactors = FALSE)

    expect_warning(
        result <- resolve_cultivar("unknown cultivar", "wheat", cultivar_db = cultivar_db, use_ai = FALSE),
        "Unable to resolve cultivar"
    )

    expect_equal(result$cultivar, "unknown")
    expect_equal(result$source, "unresolvable")
    expect_equal(result$raw_input, "unknown cultivar")
    expect_equal(
        result$assumptions,
        "LLM fallback disabled and input could not be resolved with deterministic rules"
    )
})

test_that("resolve_cultivar uses AI fallback when needed", {
    cultivar_db <- data.frame(name = c("Scepter", "Mace"), stringsAsFactors = FALSE)

    with_mocked_bindings(
        .resolve_cultivar_with_ai = function(raw_input, supported_cultivars, use_cache = TRUE) {
            list(
                cultivar = "mace",
                source = "ai_resolution",
                raw_input = raw_input,
                assumptions = "AI fallback used to resolve cultivar",
                use_cache = use_cache,
                supported_cultivars = supported_cultivars
            )
        },
        {
            result <- resolve_cultivar(
                "Maybe mace?",
                "wheat",
                cultivar_db = cultivar_db,
                use_cache = FALSE
            )

            expect_equal(result$cultivar, "mace")
            expect_equal(result$source, "ai_resolution")
            expect_equal(result$raw_input, "maybe mace?")
            expect_equal(result$assumptions, "AI fallback used to resolve cultivar")
            expect_false(result$use_cache)
            expect_equal(result$supported_cultivars, c("scepter", "mace"))
        }
    )
})

test_that("cache_apsim_cultivars errors for non-raw APSIM resource URL", {
    expect_error(
        cache_apsim_cultivars("https://github.com/APSIMInitiative/ApsimX"),
        "apsim_resources_dir must be a raw GitHub URL"
    )
})

test_that("cache_apsim_cultivars skips existing cache files when overwrite is FALSE", {
    base_dir <- tempfile(pattern = "agrillm-cultivars-")
    dir.create(base_dir)
    on.exit(unlink(base_dir, recursive = TRUE, force = TRUE), add = TRUE)

    save_path <- file.path(base_dir, "wheat_cultivars.rds")
    meta_path <- file.path(base_dir, "wheat_metadata.json")
    expected <- data.frame(name = c("Scepter", "Mace"), stringsAsFactors = FALSE)
    saveRDS(expected, save_path)
    writeLines("{}", meta_path)

    expect_message(
        result <- cache_apsim_cultivars(
            "https://raw.githubusercontent.com/APSIMInitiative/ApsimX/main/Models/Resources",
            crop = "wheat",
            dest_dir = base_dir,
            overwrite = FALSE
        )
    )

    expect_equal(normalizePath(result, winslash = "/", mustWork = FALSE),
                 normalizePath(base_dir, winslash = "/", mustWork = FALSE))
    expect_equal(readRDS(save_path), expected)
    expect_true(file.exists(meta_path))
})

test_that("get_cached_cultivars reads cultivar data from the configured cache directory", {
    base_dir <- tempfile(pattern = "agrillm-cultivars-")
    cache_dir <- file.path(base_dir, "cultivars")
    dir.create(cache_dir, recursive = TRUE)
    on.exit({
        agrillm_options$reset()
        unlink(base_dir, recursive = TRUE, force = TRUE)
    }, add = TRUE)

    agrillm_options$set(cache_dir = base_dir)

    expected <- data.frame(name = c("Scepter", "Mace"), stringsAsFactors = FALSE)
    saveRDS(expected, file.path(cache_dir, "wheat_cultivars.rds"))

    result <- get_cached_cultivars("wheat")

    expect_equal(result, expected)
})

test_that("get_cached_cultivars errors when no cached cultivars exist", {
    base_dir <- tempfile(pattern = "agrillm-cultivars-")
    dir.create(base_dir)
    on.exit({
        agrillm_options$reset()
        unlink(base_dir, recursive = TRUE, force = TRUE)
    }, add = TRUE)

    agrillm_options$set(cache_dir = base_dir)

    expect_error(
        get_cached_cultivars("wheat"),
        "No cached cultivars found for crop 'wheat'"
    )
})
