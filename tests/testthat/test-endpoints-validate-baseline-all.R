context("endpoints-validate-baseline")

test_that("endpoint_validate_baseline_combined correctly validates data", {
  input <- validate_baseline_all_input(file.path("testdata", "Malawi2019.PJNZ"),
                                       file.path("testdata", "malawi.geojson"),
                                       file.path("testdata", "population.csv"))
  response <- validate_baseline_combined(input)
  expect_equal(response$consistent, scalar(TRUE))
})

test_that("endpoint_validate_baseline_combined returns error on invalid data", {
  input <- validate_baseline_all_input(file.path("testdata", "Malawi2019.PJNZ"),
                                       file.path("testdata", "malawi.geojson"),
                                       file.path("testdata", "population.csv"))
  mock_read_iso3 <- mockery::mock("BLZ")
  with_mock("hintr:::read_iso3" = mock_read_iso3, {
    error <- expect_error(validate_baseline_combined(input))
  })
  expect_equal(error$data[[1]]$error, scalar("INVALID_BASELINE"))
  expect_equal(
    error$data[[1]]$detail,
    scalar("Countries aren't consistent got MWI from pjnz and BLZ from shape."))
  expect_equal(error$status_code, 400)
})

test_that("validation works with muliple PJNZ files", {
  skip_if_sensitive_data_missing()
  input <- validate_baseline_all_input(
    file.path("testdata", "sensitive", "ZMB", "data",
              "zmb_all_pjnz_extract.zip"),
    file.path("testdata", "sensitive", "ZMB", "data",
              "zmb_areas.geojson"),
    NULL)
  response <- validate_baseline_combined(input)
  expect_equal(response$consistent, scalar(TRUE))
})

test_that("validation works with inconsistent data", {
  skip_if_sensitive_data_missing()
  input <- validate_baseline_all_input(
    file.path("testdata", "sensitive", "ZMB", "data",
              "zmb_all_pjnz_extract.zip"),
    file.path("testdata", "malawi.geojson"),
    NULL)
  error <- expect_error(validate_baseline_combined(input))
  expect_equal(error$data[[1]]$error, scalar("INVALID_BASELINE"))
  expect_equal(
    error$data[[1]]$detail,
    scalar("Countries aren't consistent got ZMB from pjnz and MWI from shape."))
  expect_equal(error$status_code, 400)
})

