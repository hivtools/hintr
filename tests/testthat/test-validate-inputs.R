context("validate-inputs")

test_that("baseline inputs can be validated and return data", {
  pjnz <- file.path("testdata", "Botswana2018.PJNZ")
  ## TODO: Expand validation to include other input files
  expect_equal(do_validate_pjnz(pjnz), list(country = scalar("Botswana")))

  mock_read_country <- mockery::mock("GBR")
  with_mock("hintr:::read_country" = mock_read_country, {
    expect_error(do_validate_pjnz(pjnz), "Invalid country")
  })
})

test_that("country can be read from PJNZ file", {
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
  expect_s3_class(json, "json")
})

test_that("do_validate_population validates population file", {
  population <- file.path("testdata", "population.csv")
  pop <- do_validate_population(population)
  ## No actual data to return but has been validated
  expect_true(is.na(pop))
})

test_that("do_validate_programme validates programme file", {
  programme <- file.path("testdata", "programme.csv")
  data <- do_validate_programme(programme)
  ## Some arbitrary test that the data has actually been returned
  expect_true(nrow(data) > 1400)
  expect_equal(typeof(data$value), "integer")
})
