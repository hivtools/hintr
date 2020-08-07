context("endpoints-validate-baseline")

## Force a garbage collect here, as otherwise we get a really
## difficult to deal with error.
gc()

test_that("endpoint_validate_baseline_combined correctly validates data", {
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
})

test_that("endpoint_validate_baseline_combined returns error on invalid data", {
  pjnz <- file.path("testdata", "Malawi2019.PJNZ")
  shape <- file.path("testdata", "malawi.geojson")
  population <- file.path("testdata", "population.csv")
  req <- list(postBody =
                '{"pjnz": "path/to/file",
                  "shape": "path/to/file",
                  "population": "path/to/file"}')
  mock_read_iso3 <- mockery::mock("BLZ")
  with_mock("hintr:::read_iso3" = mock_read_iso3, {
    res <- MockPlumberResponse$new()
    response <- endpoint_validate_baseline_combined(req, res, pjnz, shape,
                                                    population)
    response <- jsonlite::parse_json(response)
  })
  expect_equal(response$status, "failure")
  expect_length(response$errors, 1)
  expect_equal(response$errors[[1]]$error, "INVALID_BASELINE")
  expect_equal(response$errors[[1]]$detail,
               "Countries aren't consistent got MWI from pjnz and BLZ from shape.")
  expect_equal(response$data, structure(list(), names = character(0)))
  expect_equal(res$status, 400)
})

test_that("validation works with muliple PJNZ files", {
  skip_if_sensitive_data_missing()
  pjnz <- file.path("testdata", "sensitive", "ZMB", "data",
                    "zmb_all_pjnz_extract.zip")
  shape <- file.path("testdata", "sensitive", "ZMB", "data",
                     "zmb_areas.geojson")
  req <- list(postBody =
                '{"pjnz": "path/to/file",
                  "shape": "path/to/file",
                  "population": "path/to/file"}')
  res <- MockPlumberResponse$new()
  response <- endpoint_validate_baseline_combined(req, res, pjnz, shape, NULL)
  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "success")
  expect_equal(response$errors, list())
  expect_equal(response$data$consistent, TRUE)
  expect_equal(res$status, 200)
})

test_that("validation works with inconsistent ", {
  skip_if_sensitive_data_missing()
  pjnz <- file.path("testdata", "sensitive", "ZMB", "data",
                    "zmb_all_pjnz_extract.zip")
  shape <- file.path("testdata", "malawi.geojson")
  req <- list(postBody =
                '{"pjnz": "path/to/file",
                  "shape": "path/to/file",
                  "population": "path/to/file"}')
  res <- MockPlumberResponse$new()
  response <- endpoint_validate_baseline_combined(req, res, pjnz, shape, NULL)
  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "failure")
  expect_length(response$errors, 1)
  expect_equal(response$errors[[1]]$error, "INVALID_BASELINE")
  expect_equal(response$errors[[1]]$detail,
               "Countries aren't consistent got ZMB from pjnz and MWI from shape.")
  expect_equal(res$status, 400)

  skip("TODO: enable validation of shape + PJNZ region codes mrc-1156")
  res <- MockPlumberResponse$new()
  mock_read_iso3 <- mockery::mock("ZMB")
  with_mock("hintr:::read_iso3" = mock_read_iso3, {
    response <- endpoint_validate_baseline_combined(req, res, pjnz, shape, NULL)
  })
  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "failure")
  expect_length(response$errors, 1)
  expect_equal(response$errors[[1]]$error, "INVALID_BASELINE")
  expect_match(response$errors[[1]]$detail,
               "Shape file contains spectrum region codes missing from PJNZ files: 0
PJNZ files contain spectrum region codes missing from shape file: .*", perl = TRUE)
  expect_equal(res$status, 400)
})

