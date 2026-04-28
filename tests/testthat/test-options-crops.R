
# Test default crops values
test_that("agrillm_options$get('crops') returns correct defaults", {
    agrillm_options$reset()
    crops <- agrillm_options$get("crops")
    expect_type(crops, "list")
    expect_equal(crops$wheat, c("wheat", "bread wheat", "spring wheat", "winter wheat"))
    expect_equal(crops$barley, c("barley", "feed barley", "malting barley"))
    expect_equal(crops$canola, c("canola", "oilseed rape", "rapeseed"))
})

# Test set and get
test_that("agrillm_options$set updates crops and get returns new values", {
    agrillm_options$reset()
    agrillm_options$set(crops = list(
        wheat = c("wheat", "durum"),
        barley = c("barley"),
        canola = c("canola", "rapeseed")
    ))
    crops <- agrillm_options$get("crops")
    expect_equal(crops$wheat, c("wheat", "durum"))
    expect_equal(crops$barley, c("barley"))
    expect_equal(crops$canola, c("canola", "rapeseed"))
})

# Test reset restores defaults
test_that("agrillm_options$reset restores default crops", {
    agrillm_options$set(crops = list(
        wheat = c("other"),
        barley = c("other"),
        canola = c("other")
    ))
    agrillm_options$reset()
    crops <- agrillm_options$get("crops")
    expect_equal(crops$wheat, c("wheat", "bread wheat", "spring wheat", "winter wheat"))
    expect_equal(crops$barley, c("barley", "feed barley", "malting barley"))
    expect_equal(crops$canola, c("canola", "oilseed rape", "rapeseed"))
})

# Test error on invalid crops value
test_that("agrillm_options$set errors for invalid crops value", {
    agrillm_options$reset()
    expect_error(agrillm_options$set(crops = "notalist"),
                            "crops must be a list")
    # crops.wheat have to be a vector of character, not a list
    expect_error(
        agrillm_options$set(
            crops = list(
                wheat = list("A", "B")
            )
        )
    )
    # crops.wheat have to be a vector of character, not numeric
    expect_error(
        agrillm_options$set(
            crops = list(
                wheat = c(1, 2)
            )
        )
    )
})


