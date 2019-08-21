context("endpoints")

test_that("endpoint_validate_input correctly validates data", {
  pjnz <- system.file("testdata", "Botswana2018.PJNZ", package = "hintr")
  req <- list(postBody = '{"type": "pjnz", "path": "path/to/file"}')
  res <- MockPlumberResponse$new()
  response <- endpoint_validate_input(req, res, "pjnz", pjnz)
  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "success")
  expect_equal(response$data, list(country = "Botswana"))
  expect_equal(res$status, 200)

  mock_read_country <- mockery::mock("GBR")
  with_mock("hintr:::read_country" = mock_read_country, {
    res <- MockPlumberResponse$new()
    response <- endpoint_validate_input(req, res, "pjnz", pjnz)
    response <- jsonlite::parse_json(response)
    expect_equal(response$status, "failure")
    response$errors
    expect_length(response$errors, 1)
    expect_equal(response$errors[[1]]$error, "INVALID_FILE")
    expect_equal(response$errors[[1]]$detail, "Invalid country")
    expect_equal(res$status, 400)
  })
})

test_that("endpoint_validate_input validates the input and response", {
  pjnz <- system.file("testdata", "Botswana2018.PJNZ", package = "hintr")
  mock_validate_json_schema <- mockery::mock(TRUE, cycle = TRUE)
  with_mock("hintr:::validate_json_schema" = mock_validate_json_schema, {
    ret <- endpoint_validate_input(list(postBody = "request"),
                                   MockPlumberResponse$new(), "pjnz", pjnz)
  })

  mockery::expect_called(mock_validate_json_schema, 2)
  mockery::expect_args(mock_validate_json_schema, 1, "request",
                       "ValidateInputRequest")
  mockery::expect_args(mock_validate_json_schema, 2, ret,
                       "Response")
})

test_that("endpoint model run queues a model run", {
  ## Create request data
  inputs <- list(
    pjnz = "path/tp/pjnz",
    shape = "path",
    population = "path",
    survey = "path",
    programme = "path",
    anc = "path"
  )
  parameters <- list(
    max_iterations = 250,
    no_of_simulations = 3000,
    input_data = list(
      programme = TRUE,
      anc = FALSE
    )
  )
  req <- list(postBody = '
  {
    "inputs": {
      "pjnz": "path/to/file",
      "shape": "path/to/file",
      "population": "path/to/file",
      "survey": "path/to/file",
      "programme": "path/to/file",
      "anc": "path/to/file"
    },
    "options": {
      "max_iterations" : 250,
      "no_of_simulations": 3000,
      "input_data": {
        "programme": true,
        "anc": false
      }
    }
  }')

  ## Create a queue and workers
  model_queue_start(tempfile())

  ## Create mock response
  res <- MockPlumberResponse$new()

  ## Call the endpoint
  response <- endpoint_run_model(req, res, inputs, parameters)
  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "success")
  expect_true("job_id" %in% names(response$data))
  expect_equal(res$status, 200)
})

test_that("endpoint_run_status correctly returns response", {
  mock_response <- function(success, status) {
    list(success = success,
         value = list(
          status = status
         ))
  }
  mock_with_success <- mockery::mock(mock_response(TRUE, "COMPLETE"),
                                     mock_response(TRUE, "RUNNING"),
                                    mock_response(FALSE, ""))
  mock_model_queue_result <- function(job_id) {
    data.frame(x = c(1, 2, 3), y = c(3, 4, 5))
  }
  req <- list(postBody = '{"job_id":"123"}')
  res <- MockPlumberResponse$new()
  with_mock("hintr:::with_success" = mock_with_success,
            "hintr:::model_queue_result" = mock_model_queue_result, {
    response <- endpoint_run_status(req, res, "123")
    response <- jsonlite::parse_json(response)
    expect_equal(response$status, "success")
    expect_equal(response$data$job_id, "123")
    expect_true(response$data$complete)
    expect_false(is.null(response$data$result))
    expect_equal(res$status, 200)

    response <- endpoint_run_status(req, res, "123")
    response <- jsonlite::parse_json(response)
    expect_equal(response$status, "success")
    expect_equal(response$data$job_id, "123")
    expect_false(response$data$complete)
    expect_null(response$data$result)
    expect_equal(response$data$progress, "50%")
    expect_equal(response$data$timeRemaining, "10s")
    expect_equal(res$status, 200)

    response <- endpoint_run_status(req, res, "123")
    response <- jsonlite::parse_json(response)
    expect_equal(res$status, 400)
    expect_equal(response$status, "failure")
    expect_equal(response$errors[[1]]$error, "FAILED_TO_CHECK_STATUS")
    expect_equal(response$errors[[1]]$detail, "")
  })
})

test_that("hintr_response correctly prepares response", {
  value <- list(
    success = TRUE,
    value = list(country = scalar("test"))
  )
  expected_response <-
    '{"status":"success","errors":{},"data":{"country":"test"}}'
  response <- hintr_response(value)
  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "success")
  expect_equal(names(response$data), "country")
  expect_equal(response$data$country, "test")

  value <- list(
    success = FALSE,
    errors = list(list(error = scalar("INVALID_PJNZ"),
                       detail = scalar("Example error")),
                  list(error = scalar("OTHER_ERROR"),
                       detail = scalar("Second example")))
  )
  response <- hintr_response(value)
  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "failure")
  expect_length(response$errors, 2)
  expect_equal(response$errors[[1]]$error, "INVALID_PJNZ")
  expect_equal(response$errors[[1]]$detail, "Example error")
  expect_equal(response$errors[[2]]$error, "OTHER_ERROR")
  expect_equal(response$errors[[2]]$detail, "Second example")

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
  expect_equal(errors[[2]]$detail, scalar(""))
})

test_that("hintr API can be tested", {
  expect_s3_class(api_root(), "scalar")
})

test_that("plumber api can be built", {
  api <- api_build()
  expect_s3_class(api, "plumber")
  expect_length(api$routes, 4)
  expect_equal(names(api$routes), c("validate", "run_model", "run_status", ""))
})

