template_apsimx <- function() {
    apsimx_path <- testthat::test_path("..", "..", "inst", "templates", "wheat.apsimx")
    rapsimng::read_apsimx(apsimx_path)
}

test_that("apply_soil_water updates the APSIM water profile from PAWC fraction", {
    apsimx <- template_apsimx()

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

test_that("apply_sowing updates sowing date permutations and evidence", {
    apsimx <- template_apsimx()

    updated <- apply_sowing(apsimx, list(start = "01-may", end = "07-may"))
    sowing_node <- rapsimng::search_path(updated, "[Factors].Permutation.SowingDate")
    evidence <- attr(updated, "agrillm")$sowing$evidence

    expect_equal(sowing_node$node$Specification, "[Sowing].Script.SowDate=01-may, 04-may, 07-may")
    expect_equal(evidence$sowing_dates, "01-may, 04-may, 07-may")
    expect_equal(
        evidence$assumptions,
        "Assumed sowing dates based on provided start and end dates. Daily sowing dates generated for further analysis."
    )
})

test_that("apply_fertilisation_n applies an explicit fertilisation amount", {
    apsimx <- template_apsimx()

    updated <- apply_fertilisation_n(apsimx, list(value = 80))
    fertilisation_node <- rapsimng::search_path(updated, "[Factors].Permutation.Fertilisation")
    evidence <- attr(updated, "agrillm")$fertilisation_n$evidence

    expect_equal(fertilisation_node$node$Specification, "[SowingFertiliser].Script.Amount=80")
    expect_equal(evidence$amount, "80")
    expect_equal(
        evidence$assumptions,
        "Assumed fertilisation amount based on provided input. Default values used if input is missing or unknown."
    )
})

test_that("apply_fertilisation_n keeps the default scenario sequence for unknown inputs", {
    apsimx <- template_apsimx()

    updated <- apply_fertilisation_n(apsimx, list(value = "unknown"))
    fertilisation_node <- rapsimng::search_path(updated, "[Factors].Permutation.Fertilisation")

    expect_equal(
        fertilisation_node$node$Specification,
        "[SowingFertiliser].Script.Amount=0, 40, 80, 120, 160, 200, 240, 280"
    )
})

test_that("apply_cultivar applies a resolved cultivar", {
    apsimx <- template_apsimx()

    updated <- apply_cultivar(apsimx, crop = "wheat", cultivar = list(cultivar = "scepter"))
    cultivar_node <- rapsimng::search_path(updated, "[Factors].Permutation.Cultivar")
    evidence <- attr(updated, "agrillm")$cultivar$evidence

    expect_equal(cultivar_node$node$Specification, "[Sowing].Script.CultivarName=scepter")
    expect_equal(evidence$cultivar, "scepter")
    expect_null(evidence$assumptions)
})

test_that("apply_cultivar warns and applies generic cultivar list for unknown input", {
    apsimx <- template_apsimx()

    expect_warning(
        updated <- apply_cultivar(apsimx, crop = "wheat", cultivar = list(cultivar = "unknown")),
        "Cultivar information is missing or unknown"
    )
    cultivar_node <- rapsimng::search_path(updated, "[Factors].Permutation.Cultivar")
    evidence <- attr(updated, "agrillm")$cultivar$evidence

    expect_match(cultivar_node$node$Specification, "spring quick")
    expect_length(evidence$cultivar, 12)
    expect_equal(
        evidence$assumptions,
        "Cultivar information is missing or unknown. Applied generic cultivar list."
    )
})

test_that("apply_clock updates APSIM clock fields and evidence", {
    apsimx <- template_apsimx()

    updated <- apply_clock(
        apsimx,
        start = "2012-01-01T00:00:00",
        end = "2013-12-31T00:00:00"
    )
    clock_node <- rapsimng::search_path(updated, "[Clock]")
    evidence <- attr(updated, "agrillm")$clock

    expect_equal(clock_node$node$Start, "2012-01-01T00:00:00")
    expect_equal(clock_node$node$End, "2013-12-31T00:00:00")
    expect_equal(evidence$evidence$clock$start, "2012-01-01T00:00:00")
    expect_equal(evidence$evidence$clock$end, "2013-12-31T00:00:00")
    expect_equal(
        evidence$assumptions,
        "Default simulation period applied (1976-2025) to capture historical weather patterns."
    )
})

test_that("apply_weather updates the APSIM weather file reference", {
    apsimx <- template_apsimx()
    met_path <- testthat::test_path("..", "..", "inst", "templates", "wagga.met")

    updated <- apply_weather(apsimx, met_path)
    weather_node <- rapsimng::search_path(updated, "[Weather]")

    expect_equal(weather_node$node$FileName, "wagga.met")
})