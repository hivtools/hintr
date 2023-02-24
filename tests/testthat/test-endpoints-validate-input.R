test_that("validate_baseline correctly validates data", {
  input <- validate_baseline_input(file.path("testdata", "Botswana2018.PJNZ"),
                                   "pjnz")
  response <- validate_baseline(input)
  expect_equal(response$hash, scalar("12345"))
  expect_equal(response$data$country, scalar("Botswana"))
  expect_equal(response$data$iso3, scalar("BWA"))
  expect_equal(response$filename, scalar("original"))
  expect_equal(response$fromADR, scalar(FALSE))
})

test_that("validate_baseline returns useful error if file does not exist", {
  input <- validate_baseline_input("path/to/file", "pjnz")
  error <- expect_error(validate_baseline(input))
  expect_equal(error$data[[1]]$error, scalar("INVALID_FILE"))
  expect_equal(error$data[[1]]$detail,
               scalar(
                 "File at path path/to/file does not exist. Create it, or fix the path."))
  expect_equal(error$status_code, 400)
})

test_that("validate_baseline returns useful error if file has wrong extension", {
  input <- validate_baseline_input(file.path("testdata", "malawi.geojson"), "pjnz")
  error <- expect_error(validate_baseline(input))
  expect_equal(error$data[[1]]$error, scalar("INVALID_FILE"))
  expect_equal(error$data[[1]]$detail,
               scalar(
                 "File must be of type PJNZ, zip, got type geojson."))
  expect_equal(error$status_code, 400)
})

test_that("endpoint_validate_baseline validates the data in the response", {
  input <- validate_baseline_input(file.path("testdata", "Botswana2018.PJNZ"),
                                   "pjnz")
  mock_validate_json_schema <- mockery::mock(TRUE, cycle = TRUE)
  with_mock(validate_json_schema = mock_validate_json_schema, {
    response <- validate_baseline(input)
  })

  mockery::expect_called(mock_validate_json_schema, 1)
  mockery::expect_args(mock_validate_json_schema, 1, hintr:::to_json(response),
                       "PjnzResponseData", "data")
})

test_that("endpoint_validate_baseline can take zip of PJNZ extracts", {
  skip_if_sensitive_data_missing()
  input <- validate_baseline_input(
    file.path("testdata", "sensitive", "ZMB", "data",
              "zmb_all_pjnz_extract.zip"),
    "pjnz")
  response <- validate_baseline(input)
  expect_equal(response$hash, scalar("12345"))
  expect_equal(response$data$country, scalar("Zambia"))
  expect_equal(response$data$iso3, scalar("ZMB"))
  expect_equal(response$filename, scalar("original"))
  expect_equal(response$fromADR, scalar(FALSE))
})

test_that("error thrown if zip contains non PJNZ files", {
  input <- validate_baseline_input(file.path("testdata", "invalid_files.zip"),
                                   "pjnz")
  error <- expect_error(validate_baseline(input))
  expect_equal(error$data[[1]]$error, scalar("INVALID_FILE"))
  expect_equal(error$data[[1]]$detail, scalar("Zip contains no PJNZ files"))
  expect_equal(error$status_code, 400)
})

test_that("error thrown if zip contains different countries", {
  input <- validate_baseline_input(
    file.path("testdata", "mixed_pjnz_countries.zip"),
    "pjnz")
  error <- expect_error(validate_baseline(input))
  expect_equal(error$data[[1]]$error, scalar("INVALID_FILE"))
  expect_equal(error$data[[1]]$detail, scalar(
    "Zip contains PJNZs for mixed countries, got Botswana, Malawi"))
  expect_equal(error$status_code, 400)
})

test_that("error thrown if more than 1 country with a 0 spectrum region code", {
  skip_if_sensitive_data_missing()
  input <- validate_baseline_input(
    file.path("testdata", "sensitive", "ZMB", "data",
              "zmb_all_pjnz_extract.zip"),
    "pjnz")
  mock_region_code <- mockery::mock(0, cycle = TRUE)
  with_mock(read_spectrum_region_code = mock_region_code, {
    error <- expect_error(validate_baseline(input))
  })
  expect_equal(error$data[[1]]$error, scalar("INVALID_FILE"))
  expect_match(error$data[[1]]$detail, scalar(paste0(
    "Zip contains 10 PJNZ files with spectrum region code 0. Should be max 1",
    " PJNZ with spectrum region code 0 got:\n.*")))
  expect_equal(error$status_code, 400)
})

test_that("validate_baseline supports shape files", {
  input <- validate_baseline_input(file.path("testdata", "malawi.geojson"),
                                   "shape")
  response <- validate_baseline(input)
  expect_equal(response$filename, scalar("original"))
  expect_equal(response$hash, scalar("12345"))
  data <- jsonlite::fromJSON(response$data)
  expect_true(all(c("type", "features") %in% names(data)))
  expect_equal(nrow(data$features), 69)
  expect_equal(names(response$filters), c("regions", "level_labels"))
})

test_that("endpoint_validate_baseline returns human readable error", {
  gc()
  input <- validate_baseline_input(file.path("testdata", "uganda.geojson"),
                                   "shape")
  error <- expect_error(validate_baseline(input))

  expect_equal(error$data[[1]]$error, scalar("INVALID_FILE"))
  expect_equal(error$data[[1]]$detail, scalar(
    "Property area_sort_order is incorrect type, should be numeric."))
  expect_equal(error$status_code, 400)
})

test_that("can have null spectrum region code for country level region", {
  ## Regression test for bug mrc-1169
  skip_if_sensitive_data_missing()
  input <- validate_baseline_input(
    file.path("testdata", "sensitive", "ZWE", "data",
              "zwe_areas.geojson"),
    "shape")
  response <- validate_baseline(input)
  expect_equal(response$filename, scalar("original"))
  expect_equal(response$hash, scalar("12345"))
  data <- jsonlite::fromJSON(response$data)
  expect_true(all(c("type", "features") %in% names(data)))
  expect_equal(names(response$filters), c("regions", "level_labels"))
  expect_equal(response$fromADR, scalar(FALSE))
})

test_that("endpoint_validate_baseline supports population file", {
  input <- validate_baseline_input(file.path("testdata", "population.csv"),
                                   "population")
  response <- validate_baseline(input)

  expect_equal(response$filename, scalar("original"))
  expect_equal(response$hash, scalar("12345"))
  expect_equal(response$data, json_null())
  expect_equal(response$fromADR, scalar(FALSE))
})
