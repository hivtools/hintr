context("endpoints-validate")

test_that("endpoint_validate_input correctly validates data", {
  pjnz <- file.path("testdata", "Botswana2018.PJNZ")
  req <- list(postBody = '{"type": "pjnz", "path": "path/to/file"}')
  res <- MockPlumberResponse$new()
  response <- endpoint_validate_input(req, res, "pjnz", pjnz)
  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "success")
  expect_equal(response$data$filename, "Botswana2018.PJNZ")
  expect_equal(response$data$data$country, "Botswana")
  expect_equal(res$status, 200)
})

test_that("endpoint_validate_input returns error on invalid data", {
  pjnz <- file.path("testdata", "Botswana2018.PJNZ")
  req <- list(postBody = '{"type": "pjnz", "path": "path/to/file"}')
  mock_read_country <- mockery::mock("GBR")
  with_mock("hintr:::read_country" = mock_read_country, {
    res <- MockPlumberResponse$new()
    response <- endpoint_validate_input(req, res, "pjnz", pjnz)
    response <- jsonlite::parse_json(response)
    expect_equal(response$status, "failure")
    expect_length(response$errors, 1)
    expect_equal(response$errors[[1]]$error, "INVALID_FILE")
    expect_equal(response$errors[[1]]$detail, "Invalid country")
    expect_equal(res$status, 400)
  })
})

test_that("endpoint_validate_input returns nice error if file does not exist", {
  req <- list(postBody = '{"type": "pjnz", "path": "path/to/file"}')
  res <- MockPlumberResponse$new()
  response <- endpoint_validate_input(req, res, "pjnz", "path/to/file")
  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "failure")
  expect_length(response$errors, 1)
  expect_equal(response$errors[[1]]$error, "INVALID_FILE")
  expect_equal(response$errors[[1]]$detail,
               "File at path path/to/file does not exist. Create it, or fix the path.")
  expect_equal(res$status, 400)

})

test_that("endpoint_validate_input validates the input and response", {
  pjnz <- file.path("testdata", "Botswana2018.PJNZ")
  mock_validate_json_schema <- mockery::mock(TRUE, cycle = TRUE)
  with_mock("hintr:::validate_json_schema" = mock_validate_json_schema, {
    ret <- endpoint_validate_input(list(postBody = "request"),
                                   MockPlumberResponse$new(), "pjnz", pjnz)
  })

  mockery::expect_called(mock_validate_json_schema, 4)
  mockery::expect_args(mock_validate_json_schema, 1, "request",
                       "ValidateInputRequest")
  mockery::expect_args(mock_validate_json_schema, 4, ret,
                       "Response")
  mockery::expect_args(mock_validate_json_schema, 3, ret,
                       "ValidateInputResponse", "data")
})

test_that("endpoint_validate_input support shape file", {
  shape <- file.path("testdata", "malawi.geojson")
  res <- MockPlumberResponse$new()
  response <- endpoint_validate_input(
    list(postBody = '{"type":"shape","path":"path/to/file"}'),
    res,
    "shape",
    shape)
  response <- jsonlite::parse_json(response)

  expect_equal(response$status, "success")
  expect_equal(response$data$filename, "malawi.geojson")
  expect_equal(names(response$data$data), c("type", "features"))
  expect_equal(length(response$data$data$features), 502)
  expect_equal(res$status, 200)
})

test_that("endpoint_validate_input supports population file", {
  population <- file.path("testdata", "population.csv")
  res <- MockPlumberResponse$new()
  response <- endpoint_validate_input(
    list(postBody = '{"type":"population","path":"path/to/file"}'),
    res,
    "population",
    population)
  response <- jsonlite::parse_json(response)

  expect_equal(response$status, "success")
  expect_equal(response$data$filename, "population.csv")
  expect_length(response$data$data, 0)
  expect_equal(res$status, 200)
})

test_that("endpoint_validate_input supports programme file", {
  programme <- file.path("testdata", "programme.csv")
  res <- MockPlumberResponse$new()
  response <- endpoint_validate_input(
    list(postBody = '{"type":"programme","path":"path/to/file"}'),
    res,
    "programme",
    programme)
  response <- jsonlite::parse_json(response)

  expect_equal(response$status, "success")
  expect_equal(response$data$filename, "programme.csv")
  expect_equal(res$status, 200)
  ## Sanity check that data has been returned
  expect_true(length(response$data$data) >= 1400)
  expect_equal(typeof(response$data$data[[1]]$value), "integer")
})

test_that("endpoint_validate_input returns error on invalid programme data", {
  programme <- file.path("testdata", "malformed_programme.csv")
  res <- MockPlumberResponse$new()
  response <- endpoint_validate_input(
    list(postBody = '{"type":"programme","path":"path/to/file"}'),
    res,
    "programme",
    programme)
  response <- jsonlite::parse_json(response)

  expect_equal(response$status, "failure")
  expect_equal(res$status, 400)
  expect_equal(response$data, structure(list(), names = character(0)))
  expect_length(response$errors, 1)
  expect_equal(response$errors[[1]]$error, "INVALID_FILE")
  expect_equal(response$errors[[1]]$detail, "Data missing column area_id.")
})

test_that("endpoint_validate_input supports ANC file", {
  anc <- file.path("testdata", "anc.csv")
  res <- MockPlumberResponse$new()
  response <- endpoint_validate_input(
    list(postBody = '{"type":"anc","path":"path/to/file"}'),
    res,
    "anc",
    anc)
  response <- jsonlite::parse_json(response)

  expect_equal(response$status, "success")
  expect_equal(response$data$filename, "anc.csv")
  expect_equal(res$status, 200)
  ## Sanity check that data has been returned
  expect_true(length(response$data$data) >= 800)
  expect_equal(typeof(response$data$data[[1]]$value), "double")
})

test_that("endpoint_validate_input returns error on invalid ANC data", {
  anc <- file.path("testdata", "malformed_anc.csv")
  res <- MockPlumberResponse$new()
  response <- endpoint_validate_input(
    list(postBody = '{"type":"anc","path":"path/to/file"}'),
    res,
    "anc",
    anc)
  response <- jsonlite::parse_json(response)

  expect_equal(response$status, "failure")
  expect_equal(res$status, 400)
  expect_equal(response$data, structure(list(), names = character(0)))
  expect_length(response$errors, 1)
  expect_equal(response$errors[[1]]$error, "INVALID_FILE")
  expect_equal(response$errors[[1]]$detail,
    "Anc file contains regions for more than one country. Got countries MWI, AGO.")
})

test_that("endpoint_validate_input supports survey file", {
  survey <- file.path("testdata", "survey.csv")
  res <- MockPlumberResponse$new()
  response <- endpoint_validate_input(
    list(postBody = '{"type":"survey","path":"path/to/file"}'),
    res,
    "survey",
    survey)
  response <- jsonlite::parse_json(response)

  expect_equal(response$status, "success")
  expect_equal(response$data$filename, "survey.csv")
  expect_equal(res$status, 200)
  ## Sanity check that data has been returned
  expect_true(length(response$data$data) >= 30000)
  expect_equal(typeof(response$data$data[[1]]$value), "double")
})

test_that("endpoint_validate_input returns error on invalid survey data", {
  survey <- file.path("testdata", "malformed_survey.csv")
  res <- MockPlumberResponse$new()
  response <- endpoint_validate_input(
    list(postBody = '{"type":"survey","path":"path/to/file"}'),
    res,
    "survey",
    survey)
  response <- jsonlite::parse_json(response)

  expect_equal(response$status, "failure")
  expect_equal(res$status, 400)
  expect_equal(response$data, structure(list(), names = character(0)))
  expect_length(response$errors, 1)
  expect_equal(response$errors[[1]]$error, "INVALID_FILE")
  expect_equal(response$errors[[1]]$detail, "Data missing column survey_id.")
})
