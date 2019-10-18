context("endpoints-model")

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
  options = list(
    programme = TRUE,
    anc = FALSE,
    sleep = 3
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
              "programme": true,
              "anc": false,
              "sleep": 3
              }
              }')

  ## Create mock response
  res <- MockPlumberResponse$new()

  ## Call the endpoint
  queue <- Queue$new()
  model_submit <- endpoint_model_submit(queue)
  response <- model_submit(req, res, data, options)
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
  result <- queue$queue$task_wait(response$data$id)
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
  expect_equal(names(result$data), c("data", "filters"))
  expect_equal(names(result$data$data[[1]]),
               c("area_id", "sex", "age_group_id", "quarter_id", "indicator_id",
                 "mode", "mean", "lower", "upper"))
  expect_length(result$data$data, 42021)
  expect_equal(names(result$data$filters), c("age", "quarter", "indicators"))
  expect_length(result$data$filters$age, 29)
  expect_length(result$data$filters$quarter, 1)
  expect_equal(result$data$filters$quarter[[1]]$name, "Jan-Mar 2016")
  expect_length(result$data$filters$indicators, 7)
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
  options = list(
  programme = TRUE,
  anc = FALSE
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
              }
              }')

  ## Create mocks
  res <- MockPlumberResponse$new()
  queue <- Queue$new()
  mock_submit <- function(data, options) { stop("Failed to queue") }

  ## Call the endpoint
  model_submit <- endpoint_model_submit(queue)
  mockery::stub(model_submit, "queue$submit", mock_submit)
  response <- model_submit(req, res, data, options)
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
  options = list(
    programme = TRUE,
    anc = FALSE,
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
              "options": {
              "programme": true,
              "anc": false,
              "sleep": 10
              }
              }')

  ## Create mock response
  res <- MockPlumberResponse$new()

  ## Call the endpoint
  queue <- Queue$new()
  model_submit <- endpoint_model_submit(queue)
  response <- model_submit(req, res, data, options)
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
  expect_equal(result$errors[[1]]$detail, "Error in Sys.sleep(options$sleep): invalid 'time' value\n")
})
