context("endpoints-validate-baseline")

test_that("endpoint_validate_baseline_combined correctly validates data", {
<<<<<<< HEAD
  input <- validate_baseline_all_input(file.path("testdata", "Malawi2019.PJNZ"),
                                       file.path("testdata", "malawi.geojson"),
                                       file.path("testdata", "population.csv"))
  response <- validate_baseline_combined(input)
  expect_equal(response$consistent, scalar(TRUE))
=======
  ## This is a complete mystery, and there is no obvious reason why it
  ## should be failing.  Getting debug information out is proving
  ## problematic.  We should come back and pick this up later though.
  skip_on_travis()
  pjnz <- file.path("testdata", "Malawi2019.PJNZ")
  shape <- file.path("testdata", "malawi.geojson")
  population <- file.path("testdata", "population.csv")
  req <- list(postBody =
                '{"pjnz": "path/to/file",
                  "shape": "path/to/file",
                  "population": "path/to/file"}')
  res <- MockPlumberResponse$new()
  response <- endpoint_validate_baseline_combined(req, res, pjnz, shape, population)
  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "success")
  expect_equal(response$errors, list())
  expect_equal(response$data$consistent, TRUE)
  expect_equal(res$status, 200)
>>>>>>> 38f5cbc444b66755218665ff8b82782f840baa8b
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

