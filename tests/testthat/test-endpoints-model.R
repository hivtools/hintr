context("endpoints-model")

test_that("endpoint model run queues a model run", {
  test_redis_available()
  test_mock_model_available()
  ## Create request data
  data <- list(
    pjnz = "path/to/pjnz",
    shape = "path",
    population = "path",
    survey = "path",
    programme = "path",
    anc = "path"
  )
  options = list()
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
              "options": {}
              }')

  ## Create mock response
  res <- MockPlumberResponse$new()

  ## Call the endpoint
  queue <- Queue$new()
  model_submit <- endpoint_model_submit(queue)
  response <- model_submit(req, res, data, options, cfg$version_info)
  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "success")
  expect_true("id" %in% names(response$data))
  expect_equal(res$status, 200)

  ## Wait for complete and query for status
  ## Query for status
  testthat::try_again(5, {
    result <- queue$queue$task_wait(response$data$id)
    res <- MockPlumberResponse$new()
    model_status <- endpoint_model_status(queue)
    status <- model_status(NULL, res, response$data$id)
    status <- jsonlite::parse_json(status)
    expect_equal(res$status, 200)
    expect_equal(status$status, "success")
    expect_equal(status$data$id, response$data$id)
    expect_equal(status$data$done, TRUE)
    expect_equal(status$data$status, "COMPLETE")
    expect_equal(status$data$queue, 0)
    expect_equal(status$data$success, TRUE)
    expect_length(status$data$progress, 2)
    expect_equal(status$data$progress[[1]]$name, "Started mock model")
    expect_true(status$data$progress[[1]]$complete)
    expect_equal(status$data$progress[[2]]$name, "Finished mock model")
    expect_false(status$data$progress[[2]]$complete)
    Sys.sleep(2)
  })

  ## Get the result
  res <- MockPlumberResponse$new()
  model_result <- endpoint_model_result(queue)
  result <- model_result(NULL, res, status$data$id)
  result <- jsonlite::parse_json(result)
  expect_equal(res$status, 200)
  expect_equal(names(result$data), c("data", "plottingMetadata"))
  expect_equal(names(result$data$data[[1]]),
               c("area_id", "sex", "age_group", "calendar_quarter",
                 "indicator_id", "mode", "mean", "lower", "upper"))
  expect_true(length(result$data$data) > 84042)
  expect_equal(names(result$data$plottingMetadata), c("barchart", "choropleth"))


  ## Barchart
  barchart <- result$data$plottingMetadata$barchart
  expect_equal(names(barchart), c("indicators", "filters", "defaults"))
  expect_length(barchart$filters, 4)
  expect_equal(names(barchart$filters[[1]]),
               c("id", "column_id", "label", "options", "use_shape_regions"))
  expect_equal(names(barchart$filters[[2]]),
               c("id", "column_id", "label", "options"))
  ## Choropleth has the correct filters in correct order
  filters <- lapply(barchart$filters, function(filter) {
    filter$column_id
  })
  expect_equal(filters[[1]], "area_id")
  expect_equal(filters[[2]], "calendar_quarter")
  expect_equal(filters[[3]], "sex")
  expect_equal(filters[[4]], "age_group")
  expect_length(barchart$filters[[2]]$options, 2)
  expect_equal(barchart$filters[[2]]$options[[1]]$id, "CY2018Q3")
  expect_equal(barchart$filters[[2]]$options[[1]]$label, "September 2018")
  expect_true(length(barchart$filters[[4]]$options) >= 29)
  expect_length(barchart$indicators, 10)

  ## Quarters are in descending order
  calendar_quarters <-
    lapply(barchart$filters[[2]]$options, function(option) {
      option$id
    })
  expect_equal(unlist(calendar_quarters),
               sort(unlist(calendar_quarters), decreasing = TRUE))


  ## Barchart indicators are in numeric id order
  indicators <- lapply(barchart$indicators, function(indicator) {
    indicator$indicator
  })
  expect_equal(unlist(indicators),
               c("population", "prevalence", "plhiv", "art_coverage",
                 "current_art", "receiving_art", "incidence", "new_infections",
                 "anc_prevalence", "anc_art_coverage"))

  ## Choropleth
  choropleth <- result$data$plottingMetadata$choropleth
  expect_equal(names(choropleth), c("indicators", "filters"))
  expect_length(choropleth$filters, 4)
  expect_equal(names(choropleth$filters[[1]]),
               c("id", "column_id", "label", "options", "use_shape_regions"))
  expect_equal(names(choropleth$filters[[2]]),
               c("id", "column_id", "label", "options"))
  ## Choropleth has the correct filters in correct order
  filters <- lapply(choropleth$filters, function(filter) {
    filter$column_id
  })
  expect_equal(filters[[1]], "area_id")
  expect_equal(filters[[2]], "calendar_quarter")
  expect_equal(filters[[3]], "sex")
  expect_equal(filters[[4]], "age_group")
  expect_length(choropleth$filters[[2]]$options, 2)
  expect_equal(choropleth$filters[[2]]$options[[1]]$id, "CY2018Q3")
  expect_equal(choropleth$filters[[2]]$options[[1]]$label, "September 2018")
  expect_true(length(choropleth$filters[[4]]$options) >= 29)
  expect_length(choropleth$indicators, 10)

  ## Quarters are in descending order
  calendar_quarters <-
    lapply(choropleth$filters[[2]]$options, function(option) {
      option$id
    })
  expect_equal(unlist(calendar_quarters),
               sort(unlist(calendar_quarters), decreasing = TRUE))

  ## Choropleth indicators are in numeric id order
  indicators <- lapply(choropleth$indicators, function(indicator) {
    indicator$indicator
  })
  expect_equal(unlist(indicators),
               c("population", "prevalence", "plhiv", "art_coverage",
                 "current_art", "receiving_art", "incidence", "new_infections",
                 "anc_prevalence", "anc_art_coverage"))
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
  response <- model_submit(req, res, data, options, cfg$version_info)
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

test_that("querying for an orphan task returns sensible error", {
  test_redis_available()
  res <- MockPlumberResponse$new()
  queue <- Queue$new(workers = 0)
  model_result <- endpoint_model_result(queue)

  id <- ids::random_id()
  queue$queue$con$HSET(queue$queue$keys$task_status, id, "ORPHAN")

  result <- jsonlite::parse_json(model_result(NULL, res, id))
  expect_equal(res$status, 400)
  expect_equal(result$status, "failure")
  expect_length(result$data, 0)
  expect_length(result$errors, 1)
  expect_equal(result$errors[[1]]$error, "MODEL_RUN_FAILED")
  expect_equal(result$errors[[1]]$detail,
               "Worker has crashed - error details are unavailable")
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
  options = list()
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
              "use_mock_model": true
              }
              }')

  ## Create mock response
  res <- MockPlumberResponse$new()

  ## Call the endpoint
  queue <- Queue$new()
  model_submit <- endpoint_model_submit(queue)
  response <- model_submit(req, res, data, options, cfg$version_info)
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
  queue <- MockQueue$new()
  model_submit <- endpoint_model_submit(queue)
  response <- model_submit(req, res, NULL, list(), cfg$version_info)
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
  result_parsed <- jsonlite::parse_json(result)
  expect_equal(res$status, 400)

  expect_equal(result_parsed$status, "failure")
  expect_length(result_parsed$data, 0)
  expect_length(result_parsed$errors, 1)
  expect_equal(result_parsed$errors[[1]]$error, "MODEL_RUN_FAILED")
  expect_equal(result_parsed$errors[[1]]$detail, "test error")

  trace <- vcapply(result_parsed$errors[[1]]$trace, identity)
  expect_true("rrq:::rrq_worker_main()" %in% trace)
  expect_true("stop(\"test error\")" %in% trace)

  ## Check logging:
  res$headers[["Content-Type"]] <- "application/json"
  res$body <- result
  res$status <- 400
  msg <- capture_messages(
    api_log_end(NULL, NULL, res, NULL))
  expect_match(msg[[1]], "error-key: [a-z]{5}-[a-z]{5}-[a-z]{5}")
  expect_match(msg[[2]], "error-detail: test error")
  expect_match(msg[[3]], "error-trace: rrq:::rrq_worker_main")
})

test_that("running model with old version throws an error", {
  test_redis_available()

  ## Setup mocks
  res <- MockPlumberResponse$new()

  ## Call the endpoint
  queue <- Queue$new()
  model_submit <- endpoint_model_submit(queue)
  version <- list(
    hintr = "0.0.12",
    naomi = "0.0.15",
    rrq = "0.2.1"
  )
  response <- model_submit(req, res, NULL, list(), version)
  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "failure")
  expect_length(response$errors, 1)
  expect_equal(response$errors[[1]]$error, "VERSION_OUT_OF_DATE")
  expect_equal(response$errors[[1]]$detail,
               "Trying to run model with old version of options. Update model run options")
  expect_length(response$data, 0)
})
