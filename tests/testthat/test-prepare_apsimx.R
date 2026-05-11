test_that("prepare_apsimx writes an APSIM file with applied setup decisions", {
    template_file <- system.file("templates", "wheat.apsimx", package = "agrillm")
    met_file <- system.file("templates", "wagga.met", package = "agrillm")
    output_file <- tempfile(fileext = ".apsimx")
    on.exit(unlink(output_file, force = TRUE), add = TRUE)

    decisions <- list(
        location = list(
            evidence = list(
                weather = list(file = met_file),
                soil = list(file = template_file)
            )
        ),
        crop = list(crop = "wheat"),
        cultivar = list(cultivar = "scepter"),
        sowing = list(start = "01-may", end = "07-may"),
        soil_water = list(fraction = 0.5),
        soil_nitrogen = list(value = 50),
        fert_nitrogen = list(value = 80)
    )

    with_mocked_bindings(
        apply_soil = function(apsimx, file) {
            expect_equal(normalizePath(file, winslash = "/", mustWork = TRUE),
                         normalizePath(template_file, winslash = "/", mustWork = TRUE))
            apsimx
        },
        {
            result <- prepare_apsimx(
                decisions = decisions,
                template_file = template_file,
                output_file = output_file,
                overwrite = TRUE
            )

            expect_equal(result, output_file)
            expect_true(file.exists(output_file))

            apsimx <- rapsimng::read_apsimx(output_file)
            clock_node <- rapsimng::search_path(apsimx, "[Clock]")
            weather_node <- rapsimng::search_path(apsimx, "[Weather]")
            cultivar_node <- rapsimng::search_path(apsimx, "[Factors].Permutation.Cultivar")
            sowing_node <- rapsimng::search_path(apsimx, "[Factors].Permutation.SowingDate")
            fertilisation_node <- rapsimng::search_path(apsimx, "[Factors].Permutation.Fertilisation")
            water_node <- rapsimng::search_path(apsimx, "[Soil].Water")
            nitrogen_node <- rapsimng::search_path(apsimx, "[Soil].NO3")
            physical_node <- rapsimng::search_path(apsimx, "[Soil].Physical")

            thickness <- unlist(physical_node$node$Thickness) |> as.numeric()
            dul <- unlist(physical_node$node$DUL) |> as.numeric()
            ll15 <- unlist(physical_node$node$LL15) |> as.numeric()
            expected_sw <- ll15 + 0.5 * (dul - ll15)
            expected_no3 <- distribute_soil_n(50, thickness)

            expect_equal(clock_node$node$Start, "1976-01-01T00:00:00")
            expect_equal(clock_node$node$End, "2025-12-31T00:00:00")
            expect_equal(weather_node$node$FileName, "wagga.met")
            expect_equal(cultivar_node$node$Specification, "[Sowing].Script.CultivarName=scepter")
            expect_equal(sowing_node$node$Specification, "[Sowing].Script.SowDate=01-may, 04-may, 07-may")
            expect_equal(fertilisation_node$node$Specification, "[SowingFertiliser].Script.Amount=80")
            expect_equal(unlist(water_node$node$InitialValues) |> as.numeric(), expected_sw)
            expect_equal(
                unlist(nitrogen_node$node$InitialValues) |> as.numeric(),
                expected_no3,
                tolerance = 1e-4
            )
        }
    )
})

test_that("prepare_apsimx rejects non-apsimx output paths", {
    template_file <- system.file("templates", "wheat.apsimx", package = "agrillm")

    expect_error(
        prepare_apsimx(list(), template_file, tempfile(fileext = ".txt")),
        "Output file must have .apsimx extension"
    )
})

test_that("prepare_apsimx refuses to overwrite existing output by default", {
    template_file <- system.file("templates", "wheat.apsimx", package = "agrillm")
    output_file <- tempfile(fileext = ".apsimx")
    file.create(output_file)
    on.exit(unlink(output_file, force = TRUE), add = TRUE)

    expect_error(
        prepare_apsimx(list(), template_file, output_file, overwrite = FALSE),
        "Output file already exists and overwrite is set to FALSE"
    )
})