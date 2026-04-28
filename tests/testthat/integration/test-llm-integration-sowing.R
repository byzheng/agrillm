
test_that("resolve_sowing returns a window for 'early May' [LLM integration]", {
    skip_if_no_llm()
    result <- resolve_sowing("early May")
    expect_equal(result$type, "window")
    expect_true(!is.null(result$start) && !is.null(result$end))
    expect_equal(result$start, "01-May")
    expect_equal(result$end, "10-May")
    expect_equal(result$shift, "mid")
    expect_equal(result$source, "user_phrase")
    expect_equal(result$raw_input, "early may")
})



test_that("resolve_sowing returns a window for 'late May' [LLM integration]", {
    skip_if_no_llm()
    result <- resolve_sowing("late May")
    expect_equal(result$type, "window")
    expect_true(!is.null(result$start) && !is.null(result$end))
    expect_equal(result$start, "21-May")
    expect_equal(result$end, "31-May")
    expect_equal(result$shift, "mid")
    expect_equal(result$source, "user_phrase")
    expect_equal(result$raw_input, "late may")
})


test_that("resolve_sowing returns a window for 'May' [LLM integration]", {
    skip_if_no_llm()
    result <- resolve_sowing("May")
    expect_equal(result$type, "window")
    expect_true(!is.null(result$start) && !is.null(result$end))
    expect_equal(result$start, "01-May")
    expect_equal(result$end, "31-May")
    expect_equal(result$shift, "mid")
    expect_equal(result$source, "user_phrase")
    expect_equal(result$raw_input, "may")
})


test_that("resolve_sowing returns a window for 'I will sow in April' [LLM integration]", {
    skip_if_no_llm()
    result <- resolve_sowing("I will sow in April")
    expect_equal(result$type, "window")
    expect_true(!is.null(result$start) && !is.null(result$end))
    expect_equal(result$start, "01-Apr")
    expect_equal(result$end, "30-Apr")
    expect_equal(result$shift, "mid")
    expect_equal(result$source, "user_phrase")
    expect_equal(result$raw_input, "i will sow in april")
})

test_that("resolve_sowing returns a window for 'Before 10 May' [LLM integration]", {
    skip_if_no_llm()
    result <- resolve_sowing("Before 10 May")
    expect_equal(result$type, "window")
    expect_true(!is.null(result$start) && !is.null(result$end))
    expect_equal(result$start, "30-Apr")
    expect_equal(result$end, "10-May")
    expect_equal(result$shift, "before")
    expect_equal(result$source, "user_phrase")
    expect_equal(result$raw_input, "before 10 may")
})


