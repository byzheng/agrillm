test_that("apply_soil_water updates the APSIM water profile for fraction inputs", {
    apsimx_path <- system.file("examples", "wheat.apsimx", package = "agrillm")
    apsimx <- rapsimng::read_apsimx(apsimx_path)

    physical_node <- rapsimng::search_path(apsimx, "[Soil].Physical")
    thickness <- unlist(physical_node$node$Thickness) |> as.numeric()
    dul <- unlist(physical_node$node$DUL) |> as.numeric()
    ll15 <- unlist(physical_node$node$LL15) |> as.numeric()
    expected_sw <- ll15 + 0.5 * (dul - ll15)
    expected_evidence <- sum((expected_sw - ll15) * thickness)

    updated <- apply_soil_water(apsimx, list(fraction = 0.5))
    water_node <- rapsimng::search_path(updated, "[Soil].Water")
    evidence <- attr(updated, "agrillm")$soil_water

    expect_equal(unlist(water_node$node$Thickness) |> as.numeric(), thickness)
    expect_equal(unlist(water_node$node$InitialValues) |> as.numeric(), expected_sw)
    expect_equal(evidence$soil_water, expected_evidence)
    expect_equal(
        evidence$assumptions,
        "Assumed soil water content based on provided fraction of PAWC."
    )
})

test_that("apply_soil_water leaves the APSIM water profile unchanged for unknown inputs", {
    apsimx_path <- system.file("examples", "wheat.apsimx", package = "agrillm")
    apsimx <- rapsimng::read_apsimx(apsimx_path)

    initial_water <- rapsimng::search_path(apsimx, "[Soil].Water")
    updated <- apply_soil_water(apsimx, list(type = "unknown"))
    updated_water <- rapsimng::search_path(updated, "[Soil].Water")
    evidence <- attr(updated, "agrillm")$soil_water

    expect_equal(updated_water$node$InitialValues, initial_water$node$InitialValues)
    expect_null(evidence$soil_water)
    expect_null(evidence$assumptions)
})

