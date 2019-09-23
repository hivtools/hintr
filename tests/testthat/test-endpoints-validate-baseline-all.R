context("endpoints-validate-baseline")

test_that("endpoint_validate_baseline_all correctly validates data", {
  pjnz <- file.path("testdata", "Malawi2019.PJNZ")
  shape <- file.path("testdata", "malawi.geojson")
  population <- file.path("testdata", "population.csv")
  req <- list(postBody =
                '{"pjnz": "path/to/file",
                  "shape": "path/to/file",
                  "population": "path/to.file"}')
  res <- MockPlumberResponse$new()
  response <- endpoint_validate_baseline_all(req, res, pjnz, shape, population)
  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "success")
  expect_equal(response$errors, list())
  expect_equal(response$data$consistent, TRUE)
  expect_equal(response$data$complete, TRUE)
  expect_equal(res$status, 200)
})

test_that("endpoint_validate_baseline_all returns error on invalid data", {
  pjnz <- file.path("testdata", "Malawi2019.PJNZ")
  shape <- file.path("testdata", "malawi.geojson")
  population <- file.path("testdata", "population.csv")
  req <- list(postBody =
                '{"pjnz": "path/to/file",
                  "shape": "path/to/file",
                  "population": "path/to.file"}')
  mock_read_iso3 <- mockery::mock("BLZ", "MWI")
  with_mock("hintr:::read_iso3" = mock_read_iso3, {
    res <- MockPlumberResponse$new()
    response <- endpoint_validate_baseline_all(req, res, pjnz, shape, population)
    response <- jsonlite::parse_json(response)
    expect_equal(response$status, "failure")
    expect_length(response$errors, 1)
    expect_equal(response$errors[[1]]$error, "INVALID_BASELINE")
    expect_equal(response$errors[[1]]$detail,
                 "Countries aren't consistent got BLZ from pjnz and MWI from shape.")
    expect_equal(response$data, structure(list(), names = character(0)))
    expect_equal(res$status, 400)
  })
})


