context("validate-inputs")

test_that("PJNZ can be validated and return data", {
  pjnz <- file.path("testdata", "Botswana2018.PJNZ")
  ## TODO: Expand validation to include other input files
  expect_equal(do_validate_pjnz(pjnz),
               list(data = list(country = scalar("Botswana")),
                    filters = scalar(NA)))
  mock_read_country <- mockery::mock("GBR")
  with_mock("hintr:::read_country" = mock_read_country, {
    expect_error(do_validate_pjnz(pjnz), "Invalid country")
  })
})

test_that("country can be read", {
  pjnz <- file.path("testdata", "Botswana2018.PJNZ")
  expect_equal(read_country(pjnz), "Botswana")
})

test_that("assert fails if more than once country in json", {
  shape <- file.path("testdata", "malawi.geojson")
  json <- hintr_geojson_read(shape)
  expect_true(assert_single_country(json, "shape"))

  ## Change a country for purpose of testing
  json$features[[1]]$properties$iso3 <- "AGO"
  expect_error(assert_single_country(json, "shape"),
               "Shape file contains regions for more than one country. Got countries AGO, MWI.")
})

test_that("assert fails if a feature is missing an area id", {
  shape <- file.path("testdata", "malawi.geojson")
  json <- hintr_geojson_read(shape)
  expect_true(assert_area_id_exists(json))

  ## Remove an ID for testing
  json$features[[1]]$properties$area_id <- NULL
  expect_error(assert_area_id_exists(json),
               "Shape file does not contain an area ID for each region. Missing ID for 1 feature.")
})

test_that("do_validate_shape validates shape and returns geojson as list", {
  shape <- file.path("testdata", "malawi.geojson")
  json <- do_validate_shape(shape)
  expect_s3_class(json$data, "json")
})

test_that("do_validate_population validates population file", {
  population <- file.path("testdata", "population.csv")
  pop <- do_validate_population(population)
  ## No actual data to return but has been validated
  expect_true(is.na(pop$data))
  expect_true(is.na(pop$filters))
})

test_that("do_validate_programme validates programme file", {
  programme <- file.path("testdata", "programme.csv")
  shape <- file.path("testdata", "malawi.geojson")
  data <- do_validate_programme(programme, shape)
  ## Some arbitrary test that the data has actually been returned
  expect_true(nrow(data$data) > 1400)
  expect_equal(typeof(data$data$value), "integer")

  expected_filters <- list(
    list(id = scalar("5"),
         name = scalar("20-24")),
    list(id = scalar("28"),
         name = scalar("50-64")),
    list(id = scalar("3"),
         name = scalar("10-14"))
  )
  expect_equal(data$filters$age, expected_filters)
})

test_that("do_validate_anc validates ANC file", {
  anc <- file.path("testdata", "anc.csv")
  shape <- file.path("testdata", "malawi.geojson")
  data <- do_validate_anc(anc, shape)
  ## Some arbitrary test that the data has actually been returned
  expect_true(nrow(data$data) > 800)
  expect_equal(typeof(data$data$value), "double")
  expected_filters <- list(
    list(id = scalar("1"),
         name = scalar("0-4")))
  expect_equal(data$filters$age, expected_filters)
})

test_that("do_validate_survey validates survey file", {
  survey <- file.path("testdata", "survey.csv")
  shape <- file.path("testdata", "malawi.geojson")
  data <- do_validate_survey(survey, shape)
  ## Some arbitrary test that the data has actually been returned
  expect_true(nrow(data$data) > 30000)
  expect_equal(typeof(data$data$value), "double")
  expected_ages <- list(id = scalar("1"),
         name = scalar("0-4"))
  expected_survey <- list(
    list(id = scalar("MWI2015DHS"),
         name = scalar("MWI2015DHS")),
    list(id = scalar("MWI2010DHS"),
         name = scalar("MWI2010DHS")),
    list(id = scalar("MWI2004DHS"),
         name = scalar("MWI2004DHS"))
  )
  expect_equal(data$filters$age[[1]], expected_ages)
  expect_length(data$filters$age, 11)
  expect_equal(data$filters$survey, expected_survey)
})

test_that("do_validate_programme returns useful error from shapefile comparison", {
  programme <- file.path("testdata", "programme.csv")
  shape <- file.path("testdata", "malawi_missing_regions.geojson")
  expect_error(do_validate_programme(programme, shape),
    "Regions aren't consistent programme file contains 28 regions missing from shape file including:\n\\w+")
})

test_that("do_validate_anc returns useful error from shapefile comparison", {
  anc <- file.path("testdata", "anc.csv")
  shape <- file.path("testdata", "malawi_missing_regions.geojson")
  expect_error(do_validate_anc(anc, shape),
    "Regions aren't consistent ANC file contains 28 regions missing from shape file including:\n\\w+")
})

test_that("do_validate_survey returns useful error from shapefile comparison", {
  survey <- file.path("testdata", "survey.csv")
  shape <- file.path("testdata", "malawi_missing_regions.geojson")
  expect_error(do_validate_survey(survey, shape),
    "Regions aren't consistent survey file contains 434 regions missing from shape file including:\n\\w+")
})

test_that("converting from numeric to iso3 works", {
  expect_equal(iso_numeric_to_alpha_3(454), "MWI")
  expect_equal(iso_numeric_to_alpha_3(084), "BLZ")
})

test_that("can read iso3", {
  pjnz <- file.path("testdata", "Malawi2019.PJNZ")
  expect_equal(read_iso3(pjnz, "pjnz"), "MWI")
  shape <- file.path("testdata", "malawi.geojson")
  expect_equal(read_iso3(shape, "shape"), "MWI")
})

test_that("can read regions", {
  shape <- file.path("testdata", "malawi.geojson")
  expect_true(all(grepl(
    "^MWI[\\.\\d]*$", read_regions(shape, "shape"), perl = TRUE)))
  population <- file.path("testdata", "population.csv")
  expect_true(all(grepl(
    "^MWI[\\.\\d]*$", read_regions(population, "population"), perl = TRUE)))
})

test_that("baseline data can be validated as a collection", {
  pjnz <- file.path("testdata", "Malawi2019.PJNZ")
  shape <- file.path("testdata", "malawi.geojson")
  population <- file.path("testdata", "population.csv")
  response <- do_validate_baseline(pjnz, shape, population)
  expect_true(response$consistent)
  expect_true(response$complete)

  response <- do_validate_baseline(NULL, shape, population)
  expect_true(response$consistent)
  expect_false(response$complete)

  response <- do_validate_baseline(pjnz, NULL, population)
  expect_true(response$consistent)
  expect_false(response$complete)

  response <- do_validate_baseline(pjnz, shape, NULL)
  expect_true(response$consistent)
  expect_false(response$complete)

  botswana_pjnz <- file.path("testdata", "Botswana2018.PJNZ")
  expect_error(do_validate_baseline(botswana_pjnz, shape, NULL))

  response <- do_validate_baseline(botswana_pjnz, NULL, population)
  expect_true(response$consistent)
  expect_false(response$complete)
})
