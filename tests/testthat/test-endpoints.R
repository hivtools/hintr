context("endpoints")

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

test_that("endpoint model run queues a model run", {
  test_redis_available()
  ## Create request data
  data <- list(
    pjnz = "path/to/pjnz",
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
    ),
    sleep = 5
  )
  req <- list(postBody = '
  {
    "data": {
      "pjnz": "path/to/file",
      "shape": "path/to/file",
      "population": "path/to/file",
      "survey": "path/to/file",
      "programme": "path/to/file",
      "anc": "path/to/file"
    },
    "parameters": {
      "max_iterations" : 250,
      "no_of_simulations": 3000,
      "options": {
        "programme": true,
        "anc": false
      },
      "sleep": 5
    }
  }')

  ## Create mock response
  res <- MockPlumberResponse$new()

  ## Call the endpoint
  queue <- Queue$new()
  model_submit <- endpoint_model_submit(queue)
  response <- model_submit(req, res, data, parameters)
  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "success")
  expect_true("id" %in% names(response$data))
  expect_equal(res$status, 200)

  ## Query for status
  res <- MockPlumberResponse$new()
  model_status <- endpoint_model_status(queue)
  status <- model_status(NULL, res, response$data$id)
  status <- jsonlite::parse_json(status)
  expect_equal(res$status, 200)
  expect_equal(status$status, "success")
  expect_equal(status$data$id, response$data$id)
  expect_equal(status$data$done, FALSE)
  expect_equal(status$data$status, "RUNNING")
  expect_equal(status$data$queue, 0)

  ## Wait for complete and query for status
  ## Query for status
  Sys.sleep(5)
  res <- MockPlumberResponse$new()
  status <- model_status(NULL, res, response$data$id)
  status <- jsonlite::parse_json(status)
  expect_equal(res$status, 200)
  expect_equal(status$status, "success")
  expect_equal(status$data$id, response$data$id)
  expect_equal(status$data$done, TRUE)
  expect_equal(status$data$status, "COMPLETE")
  expect_equal(status$data$queue, 0)
  expect_equal(status$data$success, TRUE)

  ## Get the result
  res <- MockPlumberResponse$new()
  model_result <- endpoint_model_result(queue)
  result <- model_result(NULL, res, status$data$id)
  result <- jsonlite::parse_json(result)
  expect_equal(res$status, 200)
  expect_equal(result$data, 2)
})

test_that("endpoint_run_model returns error if queueing fails", {
  test_redis_available()
  ## Create request data
  data <- list(
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
    options = list(
      programme = TRUE,
      anc = FALSE
    )
  )
  req <- list(postBody = '
    {
      "data": {
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

  ## Create mocks
  res <- MockPlumberResponse$new()
  queue <- Queue$new()
  mock_submit <- function(data, parameters) { stop("Failed to queue") }

  ## Call the endpoint
  model_submit <- endpoint_model_submit(queue)
  mockery::stub(model_submit, "queue$submit", mock_submit)
  response <- model_submit(req, res, data, parameters)
  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "failure")
  expect_length(response$errors, 1)
  expect_equal(response$errors[[1]]$error, "FAILED_TO_QUEUE")
  expect_equal(response$errors[[1]]$detail, "Failed to queue")
  expect_equal(res$status, 400)
})



test_that("querying for status of missing job returns useful message", {
  test_redis_available()

  res <- MockPlumberResponse$new()
  queue <- Queue$new()
  model_status <- endpoint_model_status(queue)
  status <- model_status(NULL, res, "ID")
  status <- jsonlite::parse_json(status)
  expect_equal(res$status, 200)
  expect_equal(status$status, "success")
  expect_null(status$data$done)
  expect_equal(status$data$status, "MISSING")
  expect_null(status$data$success)
  expect_equal(status$data$id, "ID")
})

test_that("querying for result of missing job returns useful error", {
  test_redis_available()

  res <- MockPlumberResponse$new()
  queue <- Queue$new()
  model_result <- endpoint_model_result(queue)
  result <- model_result(NULL, res, "ID")
  result <- jsonlite::parse_json(result)
  expect_equal(res$status, 400)
  expect_equal(result$status, "failure")
  expect_length(result$data, 0)
  expect_length(result$errors, 1)
  expect_equal(result$errors[[1]]$error, "FAILED_TO_RETRIEVE_RESULT")
  expect_equal(result$errors[[1]]$detail, "Missing some results")
})

test_that("endpoint_run_status returns error if query for status fails", {
  test_redis_available()

  ## Create mocks
  res <- MockPlumberResponse$new()
  queue <- Queue$new()
  mock_status <- function(data, parameters) { stop("Failed to get status") }

  ## Call the endpoint
  model_status <- endpoint_model_status(queue)
  mockery::stub(model_status, "queue$status", mock_status)
  response <- model_status(req, res, "ID")
  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "failure")
  expect_length(response$errors, 1)
  expect_equal(response$errors[[1]]$error, "FAILED_TO_RETRIEVE_STATUS")
  expect_equal(response$errors[[1]]$detail, "Failed to get status")
  expect_equal(res$status, 400)
})

test_that("querying for result of incomplete jobs returns useful error", {
  test_redis_available()
  data <- list(
    pjnz = "path/to/pjnz",
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
    ),
    sleep = 10
  )
  req <- list(postBody = '
  {
    "data": {
      "pjnz": "path/to/file",
      "shape": "path/to/file",
      "population": "path/to/file",
      "survey": "path/to/file",
      "programme": "path/to/file",
      "anc": "path/to/file"
    },
    "parameters": {
      "max_iterations" : 250,
      "no_of_simulations": 3000,
      "options": {
        "programme": true,
        "anc": false
      },
      "sleep": 10
    }
  }')

  ## Create mock response
  res <- MockPlumberResponse$new()

  ## Call the endpoint
  queue <- Queue$new()
  model_submit <- endpoint_model_submit(queue)
  response <- model_submit(req, res, data, parameters)
  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "success")

  ## Get result prematurely
  model_result <- endpoint_model_result(queue)
  result <- model_result(NULL, res, response$data$id)
  result <- jsonlite::parse_json(result)
  expect_equal(res$status, 400)
  expect_equal(result$status, "failure")
  expect_length(result$data, 0)
  expect_length(result$errors, 1)
  expect_equal(result$errors[[1]]$error, "FAILED_TO_RETRIEVE_RESULT")
  expect_equal(result$errors[[1]]$detail, "Missing some results")
})

test_that("erroring model run returns useful messages", {
  test_redis_available()

  res <- MockPlumberResponse$new()

  ## Call the endpoint
  queue <- Queue$new()
  model_submit <- endpoint_model_submit(queue)
  response <- model_submit(req, res, NULL, NULL)
  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "success")

  ## Get the status
  model_status <- endpoint_model_status(queue)
  status <- model_status(req, res, response$data$id)
  status <- jsonlite::parse_json(status)
  expect_equal(status$status, "success")
  expect_length(status$errors, 0)
  expect_equal(status$data$done, TRUE)
  expect_equal(status$data$status, "ERROR")
  expect_equal(status$data$success, FALSE)
  expect_equal(status$data$id, response$data$id)

  ## Get the result
  model_result <- endpoint_model_result(queue)
  result <- model_result(req, res, response$data$id)
  result <- jsonlite::parse_json(result)
  expect_equal(res$status, 400)
  expect_equal(result$status, "failure")
  expect_length(result$data, 0)
  expect_length(result$errors, 1)
  expect_equal(result$errors[[1]]$error, "MODEL_RUN_FAILED")
  expect_equal(result$errors[[1]]$detail, "Error in Sys.sleep(parameters$sleep): invalid 'time' value\n")
})

test_that("hintr_response correctly prepares response", {
  value <- list(
    success = TRUE,
    value = list(
      filename = scalar("file.pjnz"),
      type = scalar("pjnz"),
      data = list(country = scalar("Botswana"))
    )
  )

  ## NOTE: using a schema here that will work for now at least, but if
  ## that gets stricter it won't!
  response <- hintr_response(value, "ValidateInputResponse")

  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "success")
  expect_equal(response$data$filename, "file.pjnz")
  expect_equal(response$data$data$country, "Botswana")
  expect_equal(response$errors, list())

  value <- list(
    success = FALSE,
    errors = list(list(error = scalar("INVALID_PJNZ"),
                       detail = scalar("Example error")),
                  list(error = scalar("OTHER_ERROR"),
                       detail = scalar("Second example")))
  )
  response <- hintr_response(value, "ValidateInputRequest")
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

test_that("hintr_response distinguishes incorrect data schema", {
  ## This is a correct value for the ValidateInputResponse schema
  value <- list(
    success = TRUE,
    value = list(
      filename = scalar("test.pjnz"),
      type = scalar("pjnz"),
      data = list(
        country = scalar("Botswana"))
    )
  )

  expect_error(
    hintr_response(value, "ValidateInputResponse"),
    NA)
  expect_error(
    hintr_response(value, "ModelResultResponse"),
    class = "validation_error")
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
  expect_length(api$routes, 3)
  expect_equal(names(api$routes), c("validate", "model", ""))
  expect_equal(names(api$routes$model), c("submit", "status", "result"))
})

test_that("format_response_data correctly formats data and validates it", {
  mock_validate <- mockery::mock(TRUE)
  with_mock("hintr:::validate_json_schema" = mock_validate, {
    response <- input_response(list(country = scalar("Botswana")),
                               "/path/to/file.pjnz",
                               "pjnz")
  })
  expect_equal(response$data$country, scalar("Botswana"))
  expect_equal(response$filename, scalar("file.pjnz"))
  mockery::expect_called(mock_validate, 1)
  args <- mockery::mock_args(mock_validate)[[1]]
  expected_data <- list(
    "filename" = "file.pjnz",
    "type" = "pjnz",
    "data" = list(
      "country" = "Botswana"
    )
  )
  expect_equal(jsonlite::fromJSON(args[[1]]), expected_data)
  expect_equal(args[[2]], "PjnzResponseData")
  expect_equal(args[[3]], "data")
})

test_that("hintr json serializer supports splicing in json objects", {
  serializer <- serializer_json_hintr()
  req <- '{"test":"example request"}'
  res <- MockPlumberResponse$new()
  errorHandler <- function(req, res, e) {
    e$message
  }

  test <- '{"example_json":123}'
  response <- serializer(test, req, res, errorHandler)
  expect_equal(response$header, "Content-Type: application/json")
  expect_match(response$body, '["{\\"example_json\\":123}"]', fixed = TRUE)

  ## Declaring test as a json object
  response <- serializer(json_verbatim(test), req, res, errorHandler)
  expect_equal(response$header, "Content-Type: application/json")
  expect_match(response$body, test, fixed = TRUE)

  ## With a list
  test_list <- list(example_json = scalar(123))
  response <- serializer(test_list, req, res, errorHandler)
  expect_equal(response$header, "Content-Type: application/json")
  expect_match(response$body, test, fixed = TRUE)

  ## With error handling
  ## When trying to convert a value to JSON which throws an error
  response <- serializer(stop("Throw error"), req, res, errorHandler)
  expect_equal(response, "Throw error")
})

test_that("Schemas are draft-04", {
  meta <- readLines("http://json-schema.org/draft-04/schema")
  path <- system.file("schema", package = "hintr", mustWork = TRUE)
  files <- dir(path, full.names = TRUE, pattern = "\\.schema\\.json$")
  for (f in files) {
    expect_true(jsonvalidate::json_validate(f, meta))
  }
})

test_that("Schemas do not use const", {
  path <- system.file("schema", package = "hintr", mustWork = TRUE)
  files <- dir(path, full.names = TRUE, pattern = "\\.schema\\.json$")

  check1 <- function(x) {
    if ("const" %in% names(x)) {
      stop("Schema uses 'const' property and is not supported in draft-04")
    }
    if (is.recursive(x)) {
      lapply(x, check1)
    }
  }

  files <- dir(path, full.names = TRUE, pattern = "\\.schema\\.json$")
  for (f in files) {
    expect_error(check1(jsonlite::fromJSON(f)), NA, label = f)
  }
})
