context("endpoints-validate")

test_that("endpoint_validate_baseline correctly validates data", {
  pjnz <- file.path("testdata", "Botswana2018.PJNZ")
  req <- list(postBody = '{"type": "pjnz", "file": {"path": "path/to/file", "hash": "12345", "filename": "original"}}')
  res <- MockPlumberResponse$new()
  file <- list(path = pjnz, hash = "12345", filename = "original")
  response <- endpoint_validate_baseline(req, res, "pjnz", file)
  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "success")
  expect_equal(response$data$hash, "12345")
  expect_equal(response$data$data$country, "Botswana")
  expect_equal(response$data$data$iso3, "BWA")
  expect_equal(response$data$filename, "original")
  expect_equal(res$status, 200)
})

test_that("endpoint_validate_baseline returns nice error if file does not exist", {
  req <- list(postBody = '{"type": "pjnz", "file": {"path": "path/to/file", "hash": "12345", "filename": "original"}}')
  res <- MockPlumberResponse$new()
  file <- list(path = "path/to/file", hash = "12345", filename = "original")
  response <- endpoint_validate_baseline(req, res, "pjnz", file)
  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "failure")
  expect_length(response$errors, 1)
  expect_equal(response$errors[[1]]$error, "INVALID_FILE")
  expect_equal(response$errors[[1]]$detail,
               "File at path path/to/file does not exist. Create it, or fix the path.")
  expect_equal(res$status, 400)
})

test_that("endpoint_validate_baseline validates the input and response", {
  pjnz <- file.path("testdata", "Botswana2018.PJNZ")
  mock_validate_json_schema <- mockery::mock(TRUE, cycle = TRUE)
  file <- list(path = pjnz, hash = "12345", filename = "original")
  with_mock("hintr:::validate_json_schema" = mock_validate_json_schema, {
    ret <- endpoint_validate_baseline(list(postBody = "request"),
                                   MockPlumberResponse$new(), "pjnz", file)
  })

  mockery::expect_called(mock_validate_json_schema, 4)
  mockery::expect_args(mock_validate_json_schema, 1, "request",
                       "ValidateInputRequest")
  mockery::expect_args(mock_validate_json_schema, 4, ret,
                       "Response")
  mockery::expect_args(mock_validate_json_schema, 3, ret,
                       "ValidateInputResponse", "data")
})

test_that("endpoint_validate_baseline can take zip of PJNZ extracts", {
  skip_if_sensitive_data_missing()
  pjnz <- file.path("testdata", "sensitive", "ZMB", "data",
                    "zmb_all_pjnz_extract.zip")
  req <- list(postBody = '{"type": "pjnz", "file": {"path": "path/to/file", "hash": "12345", "filename": "original"}}')
  res <- MockPlumberResponse$new()
  file <- list(path = pjnz, hash = "12345", filename = "original")
  response <- endpoint_validate_baseline(req, res, "pjnz", file)
  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "success")
  expect_equal(response$data$hash, "12345")
  expect_equal(response$data$data$country, "Zambia")
  expect_equal(response$data$data$iso3, "ZMB")
  expect_equal(response$data$filename, "original")
  expect_equal(res$status, 200)
})

test_that("error thrown if zip contains non PJNZ files", {
  pjnz <- file.path("testdata", "invalid_files.zip")
  req <- list(postBody = '{"type": "pjnz", "file": {"path": "path/to/file", "hash": "12345", "filename": "original"}}')
  res <- MockPlumberResponse$new()
  file <- list(path = pjnz, hash = "12345", filename = "original")
  response <- endpoint_validate_baseline(req, res, "pjnz", file)
  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "failure")
  expect_equal(res$status, 400)
  expect_equal(response$errors[[1]]$error, "INVALID_FILE")
  expect_equal(response$errors[[1]]$detail,
    "Zip contains non PJNZ files: \ncat.mp4, invalid_file1.zip, invalid_file2.zip")
})

test_that("error thrown if zip contains different countries", {
  pjnz <- file.path("testdata", "mixed_pjnz_countries.zip")
  req <- list(postBody = '{"type": "pjnz", "file": {"path": "path/to/file", "hash": "12345", "filename": "original"}}')
  res <- MockPlumberResponse$new()
  file <- list(path = pjnz, hash = "12345", filename = "original")
  response <- endpoint_validate_baseline(req, res, "pjnz", file)
  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "failure")
  expect_equal(res$status, 400)
  expect_equal(response$errors[[1]]$error, "INVALID_FILE")
  expect_equal(response$errors[[1]]$detail,
               "Zip contains PJNZs for mixed countries, got Botswana, Malawi")
})

test_that("error thrown if more than 1 country with a 0 spectrum region code", {
  skip_if_sensitive_data_missing()
  pjnz <- file.path("testdata", "sensitive", "ZMB", "data",
                    "zmb_all_pjnz_extract.zip")
  req <- list(postBody = '{"type": "pjnz", "file": {"path": "path/to/file", "hash": "12345", "filename": "original"}}')
  res <- MockPlumberResponse$new()
  file <- list(path = pjnz, hash = "12345", filename = "original")
  mock_region_code <- mockery::mock(0, cycle = TRUE)
  with_mock("naomi::read_spectrum_region_code" = mock_region_code, {
    response <- endpoint_validate_baseline(req, res, "pjnz", file)
  })
  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "failure")
  expect_equal(res$status, 400)
  expect_equal(response$errors[[1]]$error, "INVALID_FILE")
  expect_match(response$errors[[1]]$detail,
               "Zip contains 10 PJNZ files with spectrum region code 0. Should be max 1 PJNZ with spectrum region code 0 got:\n.*")
})

## Forcing gc now prevents travis test failure (mrc-1265 for details)
gc()

test_that("endpoint_validate_baseline support shape file", {
  shape <- file.path("testdata", "malawi.geojson")
  res <- MockPlumberResponse$new()
  file <- list(path = shape, hash = "12345", filename = "original")
  response <- endpoint_validate_baseline(
    list(postBody = '{"type":"shape", "file": {"path": "path/to/file", "hash": "12345", "filename": "original"}}'),
    res,
    "shape",
    file)
  response <- jsonlite::parse_json(response)

  expect_equal(response$status, "success")
  expect_equal(response$data$filename, "original")
  expect_equal(response$data$hash, "12345")
  expect_true(all(c("type", "features") %in% names(response$data$data)))
  expect_equal(length(response$data$data$features), 69)
  expect_equal(res$status, 200)
  expect_equal(names(response$data$filters), c("regions", "level_labels"))
})

test_that("endpoint_validate_baseline returns human readable error", {
  shape <- file.path("testdata", "uganda.geojson")
  res <- MockPlumberResponse$new()
  file <- list(path = shape, hash = "12345", filename = "original")
  response <- endpoint_validate_baseline(
    list(postBody = '{"type":"shape", "file": {"path": "path/to/file", "hash": "12345", "filename": "original"}}'),
    res,
    "shape",
    file)
  response <- jsonlite::parse_json(response)

  expect_equal(response$status, "failure")
  expect_equal(res$status, 400)
  expect_length(response$errors, 1)
  expect_equal(response$errors[[1]]$error, "INVALID_FILE")
  expect_equal(response$errors[[1]]$detail,
               "Property area_sort_order is incorrect type, should be numeric.")
})

test_that("country can have null spectrum region code for country level region", {
  ## Regression test for bug mrc-1169
  skip_if_sensitive_data_missing()
  shape <- file.path("testdata", "sensitive", "ZWE", "data",
                     "zwe_areas.geojson")
  req <- list(postBody = '{"type": "shape", "file": {"path": "path/to/file", "hash": "12345", "filename": "original"}}')
  res <- MockPlumberResponse$new()
  file <- list(path = shape, hash = "12345", filename = "original")
  response <- endpoint_validate_baseline(req, res, "shape", file)
  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "success")
  expect_equal(response$data$hash, "12345")
  expect_true(all(c("type", "features") %in% names(response$data$data)))
  expect_equal(names(response$data$filters), c("regions", "level_labels"))
  expect_equal(res$status, 200)
})

test_that("endpoint_validate_baseline supports population file", {
  population <- file.path("testdata", "population.csv")
  res <- MockPlumberResponse$new()
  file <- list(path = population, hash = "12345", filename = "original")
  response <- endpoint_validate_baseline(
    list(postBody = '{"type":"population","file": {"path": "path/to/file", "hash": "12345", "filename": "original"}}'),
    res,
    "population",
    file)
  response <- jsonlite::parse_json(response)

  expect_equal(response$status, "success")
  expect_equal(response$data$filename, "original")
  expect_equal(response$data$hash, "12345")
  expect_length(response$data$data, 0)
  expect_equal(res$status, 200)
})
