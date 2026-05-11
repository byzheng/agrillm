test_that("distribute_soil_n conserves total and returns non-negative values", {
    total_n <- 50
    thickness <- c(100, 100, 100, 200, 300, 400)

    methods <- c("uniform", "exponential", "truncated")

    for (method in methods) {
        result <- if (method == "truncated") {
            distribute_soil_n(total_n, thickness, method = method, max_depth_mm = 600)
        } else {
            distribute_soil_n(total_n, thickness, method = method)
        }

        expect_length(result, length(thickness))
        expect_true(all(result >= 0))
        expect_equal(sum(result), total_n, tolerance = 1e-10)
    }
})

test_that("distribute_soil_n uniform method is proportional to thickness", {
    total_n <- 60
    thickness <- c(100, 200, 300)

    result <- distribute_soil_n(total_n, thickness, method = "uniform")

    expect_equal(result, c(10, 20, 30), tolerance = 1e-12)
})

test_that("distribute_soil_n truncated method allocates only above cutoff", {
    total_n <- 50
    thickness <- c(100, 100, 100, 200, 300, 400)

    result <- distribute_soil_n(total_n, thickness, method = "truncated", max_depth_mm = 600)

    expect_equal(result, c(50 / 6, 50 / 6, 50 / 6, 50 / 3, 50 / 6, 0), tolerance = 1e-10)
    expect_equal(result[6], 0)
})

test_that("distribute_soil_n exponential method declines with depth for equal layers", {
    total_n <- 40
    thickness <- rep(100, 4)

    result <- distribute_soil_n(total_n, thickness, method = "exponential", lambda_mm = 500)

    expect_true(all(diff(result) < 0))
    expect_equal(sum(result), total_n, tolerance = 1e-10)
})

test_that("distribute_soil_n handles zero total nitrogen", {
    result <- distribute_soil_n(0, c(100, 200, 300), method = "exponential")

    expect_equal(result, c(0, 0, 0))
})

test_that("distribute_soil_n validates inputs", {
    expect_error(
        distribute_soil_n(-1, c(100, 200)),
        "total_n must be a single finite non-negative number"
    )

    expect_error(
        distribute_soil_n(10, c(100, 0, 200)),
        "thickness_mm must be a non-empty numeric vector of finite positive values"
    )

    expect_error(
        distribute_soil_n(10, c(100, 200), method = "exponential", lambda_mm = 0),
        "lambda_mm must be a single finite positive number"
    )

    expect_error(
        distribute_soil_n(10, c(100, 200), method = "truncated"),
        "max_depth_mm must be supplied as a single finite positive number"
    )
})

test_that("apply_soil_nitrogen updates the APSIM NO3 profile for measured inputs", {
    apsimx_path <- system.file("templates", "wheat.apsimx", package = "agrillm")
    apsimx <- rapsimng::read_apsimx(apsimx_path)

    physical_node <- rapsimng::search_path(apsimx, "[Soil].Physical")
    thickness <- unlist(physical_node$node$Thickness) |> as.numeric()
    expected <- distribute_soil_n(50, thickness)

    updated <- apply_soil_nitrogen(apsimx, list(value = 50))
    nitrogen_node <- rapsimng::search_path(updated, "[Soil].NO3")
    evidence <- attr(updated, "agrillm")$soil_nitrogen

    expect_equal(unlist(nitrogen_node$node$Thickness) |> as.numeric(), thickness)
    expect_equal(unlist(nitrogen_node$node$InitialValues) |> as.numeric(), expected)
    expect_equal(nitrogen_node$node$InitialValuesUnits, 1)
    expect_equal(evidence$soil_nitrogen, 50)
    expect_equal(
        evidence$assumptions,
        "Assumed soil nitrogen distribution exponential along soil profile."
    )
})

test_that("apply_soil_nitrogen leaves the APSIM NO3 profile unchanged for unknown inputs", {
    apsimx_path <- system.file("templates", "wheat.apsimx", package = "agrillm")
    apsimx <- rapsimng::read_apsimx(apsimx_path)

    initial_no3 <- rapsimng::search_path(apsimx, "[Soil].NO3")
    updated <- apply_soil_nitrogen(apsimx, list(type = "unknown"))
    updated_no3 <- rapsimng::search_path(updated, "[Soil].NO3")
    evidence <- attr(updated, "agrillm")$soil_nitrogen

    expect_equal(updated_no3$node$InitialValues, initial_no3$node$InitialValues)
    expect_null(evidence$soil_nitrogen)
    expect_null(evidence$assumptions)
})
