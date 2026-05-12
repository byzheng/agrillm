test_that("recipe_registry returns registered recipe metadata", {
    registry <- recipe_registry()

    expect_type(registry, "list")
    expect_true(length(registry) >= 2)
    expect_true(all(c("recipe_read_output", "recipe_summarise_outputs") %in% names(registry)))

    for (recipe_name in names(registry)) {
        entry <- registry[[recipe_name]]

        expect_type(entry, "list")
        expect_equal(entry$name, recipe_name)
        expect_true(all(c("name", "category", "purpose", "inputs", "outputs") %in% names(entry)))
        expect_true(all(vapply(entry[c("name", "category", "purpose", "inputs", "outputs")], is.character, logical(1))))
    }
})
