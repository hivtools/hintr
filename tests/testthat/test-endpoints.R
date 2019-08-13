context("endpoints")

test_that("endpoint_validate_input correctly validates data", {
  pjnz <- system.file("testdata", "Botswana2018.PJNZ", package = "hintr")
  req <- '{"type": "pjnz", "path": "path/to/file"}'
  response <- endpoint_validate_input(req, "pjnz", pjnz)
  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "success")
  expect_equal(response$data, "Botswana")

  mock_read_country <- mockery::mock("GBR")
  with_mock("hintr:::read_country" = mock_read_country, {
    response <- endpoint_validate_input(req, "pjnz", pjnz)
    response <- jsonlite::parse_json(response)
    expect_equal(response$status, "failure")
    response$errors
    expect_length(response$errors, 1)
    expect_equal(response$errors[[1]]$error, "INVALID_FILE")
    expect_equal(response$errors[[1]]$detail, "Invalid country")
  })
})

test_that("endpoint_validate_input validates the input and response", {
  pjnz <- system.file("testdata", "Botswana2018.PJNZ", package = "hintr")
  mock_validate_json_schema <- mockery::mock(TRUE, cycle = TRUE)
  with_mock("hintr:::validate_json_schema" = mock_validate_json_schema, {
    ret <- endpoint_validate_input("request", pjnz)
  })

  mockery::expect_called(mock_validate_json_schema, 2)
  mockery::expect_args(mock_validate_json_schema, 1, "request",
                       "ValidateInputRequest")
  mockery::expect_args(mock_validate_json_schema, 2, ret,
                       "ValidateInputResponse")
})

test_that("hintr_response correctly prepares response", {
  value <- list(
    success = TRUE,
    value = scalar("Passed")
  )
  expected_response <- '{"status":"success","errors":{},"data":"Passed"}'
  response <- hintr_response(value, "Test Schema")
  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "success")
  expect_equal(response$data, "Passed")

  value <- list(
    success = FALSE,
    errors = list(list(error = "INVALID_PJNZ",
                       detail = "Example error"),
                  list(error = "OTHER_ERROR",
                       detail = "Second example"))
  )
  response <- hintr_response(value, "Test Schema")
  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "failure")
  expect_length(response$errors, 2)
  expect_length(response$errors[[1]]$error, 1)
  expect_equal(response$errors[[1]]$error[[1]], "INVALID_PJNZ")
  expect_length(response$errors[[1]]$detail, 1)
  expect_equal(response$errors[[1]]$detail[[1]], "Example error")
  expect_length(response$errors[[2]]$error, 1)
  expect_equal(response$errors[[2]]$error[[1]], "OTHER_ERROR")
  expect_length(response$errors[[2]]$detail, 1)
  expect_equal(response$errors[[2]]$detail[[1]], "Second example")

})

test_that("with_success correctly builds success message", {
  expr <- "Passed validation"
  response <- with_success(expr)
  expect_true(response$success)
  expect_equal(response$value, "Passed validation")

  expr <- function() {
    stop("Failed validation")
  }
  err <- with_success(expr())
  expect_false(err$success)
  expect_equal(err$message, "Failed validation")
  expect_equal(err$type, "simpleError")
})

test_that("hintr errors correctly formats errors", {
  errors <- hintr_errors(list(KEY1 = "value1", KEY2 = NULL))
  expect_length(errors, 2)
  expect_equal(as.character(errors[[1]]$error), "KEY1")
  expect_s3_class(errors[[1]]$error, "scalar")
  expect_equal(as.character(errors[[1]]$detail), "value1")
  expect_s3_class(errors[[1]]$detail, "scalar")
  expect_equal(as.character(errors[[2]]$error), "KEY2")
  expect_s3_class(errors[[2]]$error, "scalar")
  expect_null(errors[[2]]$detail)
})

test_that("hintr API can be tested", {
  expect_s3_class(api_root(), "scalar")
})

test_that("plumber api can be built", {
  api <- api_build()
  expect_s3_class(api, "plumber")
  expect_length(api$routes, 2)
  expect_equal(names(api$routes), c("validate", ""))
})

