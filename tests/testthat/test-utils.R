test_that(".format_list_for_ai formats nested named lists as indented lines", {
    x <- list(
        recipe = list(
            name = "recipe_read_output",
            details = list(
                category = "general",
                outputs = "data frame"
            )
        )
    )

    result <- .format_list_for_ai(x)

    expect_equal(
        result,
        paste(
            "- recipe",
            "  name: recipe_read_output",
            "  details",
            "    category: general",
            "    outputs: data frame",
            sep = "\n"
        )
    )
})

test_that(".format_list_for_ai collapses atomic vectors and prints NULL-like values", {
    x <- list(
        options = c("a", "b", "c"),
        empty = character(0),
        missing = NULL
    )

    result <- .format_list_for_ai(x)

    expect_equal(
        result,
        paste(
            "- options: a, b, c",
            "- empty: NULL",
            "- missing: NULL",
            sep = "\n"
        )
    )
})

test_that(".format_list_for_ai validates that input is a named list", {
    expect_error(.format_list_for_ai(list("a", "b")))
    expect_error(.format_list_for_ai("not a list"))
})