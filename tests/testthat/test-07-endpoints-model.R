test_that("endpoint model run queues a model run", {
  test_redis_available()
  test_mock_model_available()

  ## Setup payload
  payload <- setup_payload_submit()

  ## Call the endpoint
  queue <- test_queue(workers = 1)
  model_submit <- submit_model(queue)
  response <- model_submit(payload)
  expect_true("id" %in% names(response))

  ## Wait for complete and query for status
  ## Query for status
  result <- queue$queue$task_wait(response$id)
  status_endpoint <- queue_status(queue)
  status <- status_endpoint(response$id)
  expect_equal(status$id, response$id)
  expect_equal(status$done, scalar(TRUE))
  expect_equal(status$status, scalar("COMPLETE"))
  expect_equal(status$queue, scalar(0))
  expect_equal(status$success, scalar(TRUE))
  expect_length(status$progress, 2)
  expect_equal(status$progress[[1]]$name, scalar("Started mock model"))
  expect_true(status$progress[[1]]$complete)
  expect_equal(status$progress[[2]]$name, scalar("Finished mock model"))
  expect_false(status$progress[[2]]$complete)
  expect_equal(status$progress[[2]]$helpText, scalar("model running"))

  ## Get the result
  get_model_result <- model_result(queue)
  result <- get_model_result(response$id)
  expect_equal(result, list(
    id = scalar(response$id),
    complete = scalar(TRUE),
    warnings = list(
      list(
        text = scalar(paste0("Zero population input for 8 population groups. ",
                             "Replaced with population 0.1.")),
        locations = list(scalar("model_fit"))
      )
    )
  ))
})

test_that("endpoint_run_model returns error if queueing fails", {
  test_redis_available()
  ## Create request data
  payload <- setup_payload_submit()

  ## Create mocks
  queue <- test_queue()
  mock_submit_model_run <- function(data, options) { stop("Failed to queue") }

  ## Call the endpoint
  model_submit <- submit_model(queue)
  mockery::stub(model_submit, "queue$submit_model_run", mock_submit_model_run)
  error <- expect_error(model_submit(payload))

  expect_equal(error$data[[1]]$error, scalar("FAILED_TO_QUEUE"))
  expect_equal(error$data[[1]]$detail, scalar("Failed to queue"))
  expect_equal(error$status_code, 400)
})

test_that("running model with old version throws an error", {
  test_redis_available()

  ## Setup payload
  payload <- setup_payload_submit(version = '{
                               "hintr": "0.0.12",
                               "naomi": "0.0.15",
                               "rrq": "0.2.1"
                               }')

  ## Call the endpoint
  queue <- test_queue(workers = 1)
  model_submit <- submit_model(queue)
  error <- expect_error(model_submit(payload))

  expect_equal(error$data[[1]]$error, scalar("VERSION_OUT_OF_DATE"))
  expect_equal(error$data[[1]]$detail, scalar(
    paste0("Trying to run model with",
           " old version of options. Update model run options")))
  expect_equal(error$status_code, 400)
})

test_that("querying for status of missing job returns useful message", {
  test_redis_available()

  queue <- test_queue()
  status_endpoint <- queue_status(queue)
  status <- status_endpoint("ID")
  expect_equal(status$done, json_null())
  expect_equal(status$status, scalar("MISSING"))
  expect_equal(status$success, json_null())
  expect_equal(status$id, scalar("ID"))
})

test_that("querying for result of missing job returns useful error", {
  test_redis_available()

  queue <- test_queue()
  get_model_result <- model_result(queue)
  error <- expect_error(get_model_result("ID"))
  expect_equal(error$data[[1]]$error, scalar("FAILED_TO_RETRIEVE_RESULT"))
  expect_equal(error$data[[1]]$detail, scalar("Failed to fetch result"))
  expect_equal(error$status_code, 400)
})

test_that("querying for an orphan task returns sensible error", {
  test_redis_available()

  queue <- test_queue()
  id <- ids::random_id()
  queue$queue$con$HSET(r6_private(queue$queue)$keys$task_status, id, "DIED")
  get_model_result <- model_result(queue)
  error <- expect_error(get_model_result(id))

  expect_equal(error$data[[1]]$error, scalar("MODEL_RUN_FAILED"))
  expect_equal(error$data[[1]]$detail,
               scalar("Worker has crashed - error details are unavailable"))
  expect_equal(error$status_code, 400)
})

test_that("endpoint_run_status returns error if query for status fails", {
  test_redis_available()

  ## Create mocks
  queue <- test_queue()
  mock_status <- function(data, parameters) { stop("Failed to get status") }

  ## Call the endpoint
  status_endpoint <- queue_status(queue)
  mockery::stub(status_endpoint, "queue$status", mock_status)
  error <- expect_error(status_endpoint("ID"))
  expect_equal(error$data[[1]]$error, scalar("FAILED_TO_RETRIEVE_STATUS"))
  expect_equal(error$data[[1]]$detail, scalar("Failed to get status"))
  expect_equal(error$status_code, 400)
})

test_that("querying for result of incomplete jobs returns useful error", {
  test_redis_available()
  test_mock_model_available()

  payload <- setup_payload_submit()
  queue <- test_queue(workers = 1)
  model_submit <- submit_model(queue)
  response <- model_submit(payload)
  expect_true("id" %in% names(response))

  ## Get result prematurely
  get_model_result <- model_result(queue)
  error <- expect_error(get_model_result(response$id))

  expect_equal(error$data[[1]]$error, scalar("FAILED_TO_RETRIEVE_RESULT"))
  expect_equal(error$data[[1]]$detail,
               scalar("Failed to fetch result"))
  expect_equal(error$status_code, 400)
})

test_that("erroring model run returns useful messages", {
  test_redis_available()

  ## Call the endpoint
  queue <- MockQueue$new()
  payload <- setup_payload_submit()
  model_submit <- submit_model(queue)
  response <- model_submit(payload)
  expect_true("id" %in% names(response))
  out <- queue$queue$task_wait(response$id)

  ## Get the status
  endpoint_status <- queue_status(queue)
  status <- endpoint_status(response$id)
  expect_equal(status$done, scalar(TRUE))
  expect_equal(status$status, scalar("ERROR"))
  expect_equal(status$success, scalar(FALSE))
  expect_equal(status$id, response$id)

  # Get the result
  get_model_result <- model_result(queue)
  error <- expect_error(get_model_result(response$id))

  expect_equal(error$status_code, 400)
  expect_equal(names(error$data[[1]]), c("error", "detail", "key", "job_id"))
  expect_equal(error$data[[1]]$error, scalar("MODEL_RUN_FAILED"))
  expect_equal(error$data[[1]]$detail, scalar("test error"))
  expect_match(error$data[[1]]$key, "\\w+-\\w+-\\w+")
  expect_match(error$data[[1]]$job_id, "^[[:xdigit:]]+$")
})

test_that("model run can be cancelled", {
  test_redis_available()
  test_mock_model_available()

  ## Start the model running
  payload <- setup_payload_submit()
  queue <- test_queue(workers = 1)
  model_submit <- submit_model(queue)
  response <- model_submit(payload)
  expect_true("id" %in% names(response))
  id <- response$id

  ## Mock model run sleeps for 5, sleep here for 1 to ensure it has
  ## started and will be running
  Sys.sleep(1)
  expect_equal(queue$queue$task_status(id), setNames("RUNNING", id))

  ## Cancel the run
  worker <- queue$queue$worker_list()
  cancel_model <- model_cancel(queue)
  response <- cancel_model(id)
  expect_equal(response, json_null())

  testthat::try_again(5, {
    Sys.sleep(1)
    log <- queue$queue$worker_log_tail(worker, n = Inf)
    expect_true("CANCEL" %in% log$command)
    expect_equal(queue$queue$task_status(id), setNames("CANCELLED", id))
  })

  get_status <- queue_status(queue)
  response <- get_status(id)
  expect_true(response$done)
  expect_false(response$success)
  expect_equal(response$status, scalar("CANCELLED"))

  get_result <- model_result(queue)
  error <- expect_error(get_result(id))
  expect_equal(error$data[[1]]$error, scalar("MODEL_RUN_FAILED"))
  expect_equal(error$data[[1]]$detail,
               scalar("Model run was cancelled by user"))
  expect_equal(error$status_code, 400)
})

test_that("translation of progress", {
  test_redis_available()
  test_mock_model_available()

  payload <- setup_payload_submit()
  queue <- test_queue(workers = 1)
  model_submit <- submit_model(queue)
  get_status <- queue_status(queue)

  response <- with_hintr_language(
    "fr",
    model_submit(payload))
  id <- response$id

  ## Query for status
  result <- queue$queue$task_wait(id)
  status <- get_status(id)

  expect_equal(status$progress[[1]]$name,
               scalar("Maquette commencée"))
  expect_equal(status$progress[[2]]$name,
               scalar("Maquette terminée"))
})

test_that("error messages from naomi are translated", {
  test_redis_available()
  test_mock_model_available()
  queue <- withr::with_envvar(c("USE_MOCK_MODEL" = "false"),
                              test_queue(workers = 1))

  model_submit <- submit_model(queue)
  ## Create a population file which deliberately will cause an error
  payload <- setup_payload_submit()
  payload <- jsonlite::fromJSON(payload)
  pop <- read.csv(payload$data$population$path)
  pop$sex <- NULL
  t <- tempfile()
  write.csv(pop, t)
  payload$data$population$path <- t
  path <- tempfile()
  writeLines(jsonlite::toJSON(payload), path)

  response <- with_hintr_language(
    "fr",
    model_submit(readLines(path)))
  id <- response$id
  queue$queue$task_wait(id)

  get_result <- model_result(queue)
  error <- expect_error(get_result(id))
  expect_equal(error$data[[1]]$error, scalar("MODEL_RUN_FAILED"))
  expect_equal(error$data[[1]]$detail,
               scalar("Colonnes obligatoires introuvables: sex"))
})

test_that("failed cancel sends reasonable message", {
  test_redis_available()
  test_mock_model_available()
  ## Create request data
  queue <- test_queue()
  cancel_model <- model_cancel(queue)

  id <- ids::random_id()
  error <- expect_error(cancel_model(id))

  ## TODO: translate the message ideally - requires some work in rrq
  ## though.
  expect_equal(error$data[[1]]$error, scalar("FAILED_TO_CANCEL"))
  expect_match(error$data[[1]]$detail,
               scalar("Task [[:xdigit:]]+ is not cancelable \\(MISSING\\)"))
  expect_type(error$data[[1]]$key, "character")
  expect_equal(error$status_code, 400)
})

test_that("getting result returns empty warnings with old run", {
  test_redis_available()
  test_mock_model_available()

  ## Return v1.0.7 model results
  q <- test_queue_result(model = mock_model_v1.0.7,
                         calibrate = mock_calibrate_v1.0.7)

  endpoint <- endpoint_model_result(q$queue)
  res <- endpoint$run(q$model_run_id)
  expect_equal(res$status_code, 200)
  expect_equal(res$data$warnings, list())

  calibrate_result <- endpoint_model_calibrate_result(q$queue)
  res <- calibrate_result$run(q$calibrate_id)
  expect_equal(res$status_code, 200)
  expect_equal(res$data$warnings, list())
})
