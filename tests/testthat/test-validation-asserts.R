context("validation-asserts")

test_that("assert fails if more than once country in json", {
  shape <- file.path("testdata", "malawi.geojson")
  json <- geojsonio::geojson_read(shape, method = "local")
  expect_true(assert_single_country(json))

  ## Change a country for purpose of testing
  json$features[[1]]$properties$iso3 <- "AGO"
  expect_error(assert_single_country(json),
               "Shape file contains regions for more than one country. Got countries AGO, MWI.")
})

test_that("assert fails if a feature is missing an area id", {
  shape <- file.path("testdata", "malawi.geojson")
  json <- geojsonio::geojson_read(shape, method = "local")
  expect_true(assert_area_id_exists(json))

  ## Remove an ID for testing
  json$features[[1]]$properties$area_id <- NULL
  expect_error(assert_area_id_exists(json),
               "Shape file does not contain an area ID for each region. Missing ID for 1 feature.")
})

test_that("assert_column_names checks column names are as expected", {
  expect_true(assert_column_names(c("col1", "col2"), c("col1", "col2")))
  expect_error(assert_column_names(c("col1"), c("col1", "col2")),
               "Data missing column col2")
  expect_true(assert_column_names(c("col1", "col2"), c("col1")))
})
