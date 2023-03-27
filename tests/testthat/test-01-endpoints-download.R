test_that("spectrum download returns bytes", {
  test_mock_model_available()
  q <- test_queue_result()

  ## Submit download request
  submit <- endpoint_download_submit(q$queue)
  submit_response <- submit$run(q$calibrate_id, "spectrum")

  expect_equal(submit_response$status_code, 200)
  expect_true(!is.null(submit_response$data$id))

  ## Status
  out <- q$queue$queue$task_wait(submit_response$data$id)
  status <- endpoint_download_status(q$queue)
  status_response <- status$run(submit_response$data$id)

  expect_equal(status_response$status_code, 200)
  expect_equal(status_response$data$id, submit_response$data$id)
  expect_true(status_response$data$done)
  expect_equal(status_response$data$status, scalar("COMPLETE"))
  expect_true(status_response$data$success)
  expect_equal(status_response$data$queue, scalar(0))
  expect_length(status_response$data$progress, 1)

  ## Get result
  result <- endpoint_download_result(q$queue)
  response <- result$run(status_response$data$id)
  expect_equal(response$status_code, 200)
  expect_match(response$headers$`Content-Disposition`,
               'attachment; filename="MWI_naomi-output_\\d+-\\d+.zip"')
  size <- length(response$data)
  expect_equal(response$headers$`Content-Length`, size)

  ## Get HEAD
  head <- endpoint_download_result_head(q$queue)
  head_response <- head$run(status_response$data$id)
  expect_equal(head_response$status_code, 200)
  expect_equal(head_response$content_type, "application/octet-stream")
  expect_match(head_response$headers$`Content-Disposition`,
               'attachment; filename="MWI_naomi-output_\\d+-\\d+.zip"')
  expect_equal(head_response$headers$`Content-Length`, size)
  expect_null(head_response$body, NULL)
})

test_that("api can call spectrum download", {
  test_redis_available()
  test_mock_model_available()
  q <- test_queue_result()
  api <- api_build(q$queue)

  ## Submit download request
  submit <- api$request("POST",
                        paste0("/download/submit/spectrum/", q$calibrate_id))
  submit_body <- jsonlite::fromJSON(submit$body)
  expect_equal(submit$status, 200)
  expect_true(!is.null(submit_body$data$id))

  ## Status
  out <- q$queue$queue$task_wait(submit_body$data$id)
  status <- api$request("GET",
                        paste0("/download/status/", submit_body$data$id))

  expect_equal(status$status, 200)
  status_body <- jsonlite::fromJSON(status$body)
  expect_equal(status_body$data$id, submit_body$data$id)
  expect_true(status_body$data$done)
  expect_equal(status_body$data$status, "COMPLETE")
  expect_true(status_body$data$success)
  expect_equal(status_body$data$queue, 0)
  expect_length(status_body$data$progress, 1)

  ## Get result
  res <- api$request("GET", paste0("/download/result/", submit_body$data$id))

  expect_equal(res$status, 200)
  expect_equal(res$headers$`Content-Type`, "application/octet-stream")
  expect_match(res$headers$`Content-Disposition`,
               'attachment; filename="MWI_naomi-output_\\d+-\\d+.zip"')
  ## Size of bytes is close to expected
  size <- length(res$body)
  expect_equal(res$headers$`Content-Length`, size)

  ## Get HEAD
  head_res <- api$request("HEAD",
                          paste0("/download/result/", submit_body$data$id))

  expect_equal(head_res$status, 200)
  expect_equal(head_res$headers$`Content-Type`, "application/octet-stream")
  expect_match(head_res$headers$`Content-Disposition`,
               'attachment; filename="MWI_naomi-output_\\d+-\\d+.zip"')
  expect_equal(head_res$headers$`Content-Length`, size)
  ## Plumber uses an empty string to represent an empty body
  expect_null(head_res$body)
})

test_that("coarse output download returns bytes", {
  test_mock_model_available()
  q <- test_queue_result()

  ## Submit download request
  submit <- endpoint_download_submit(q$queue)
  submit_response <- submit$run(q$calibrate_id, "coarse_output")

  expect_equal(submit_response$status_code, 200)
  expect_true(!is.null(submit_response$data$id))

  ## Status
  out <- q$queue$queue$task_wait(submit_response$data$id)
  status <- endpoint_download_status(q$queue)
  status_response <- status$run(submit_response$data$id)

  expect_equal(status_response$status_code, 200)
  expect_equal(status_response$data$id, submit_response$data$id)
  expect_true(status_response$data$done)
  expect_equal(status_response$data$status, scalar("COMPLETE"))
  expect_true(status_response$data$success)
  expect_equal(status_response$data$queue, scalar(0))
  expect_length(status_response$data$progress, 1)

  ## Get result
  result <- endpoint_download_result(q$queue)
  response <- result$run(status_response$data$id)
  expect_equal(response$status_code, 200)
  expect_match(
    response$headers$`Content-Disposition`,
    'attachment; filename="MWI_coarse-output_\\d+-\\d+.zip"')
  size <- length(response$data)
  expect_equal(response$headers$`Content-Length`, size)
})

test_that("api can call coarse_output download", {
  test_redis_available()
  test_mock_model_available()
  q <- test_queue_result()
  api <- api_build(q$queue)

  ## Submit download request
  submit <- api$request("POST", paste0("/download/submit/coarse-output/",
                                      q$calibrate_id))
  submit_body <- jsonlite::fromJSON(submit$body)
  expect_equal(submit$status, 200)
  expect_true(!is.null(submit_body$data$id))

  ## Status
  out <- q$queue$queue$task_wait(submit_body$data$id)
  status <- api$request("GET",
                        paste0("/download/status/", submit_body$data$id))

  expect_equal(status$status, 200)
  status_body <- jsonlite::fromJSON(status$body)
  expect_equal(status_body$data$id, submit_body$data$id)
  expect_true(status_body$data$done)
  expect_equal(status_body$data$status, "COMPLETE")
  expect_true(status_body$data$success)
  expect_equal(status_body$data$queue, 0)
  expect_length(status_body$data$progress, 1)

  ## Get result
  res <- api$request("GET", paste0("/download/result/", submit_body$data$id))

  expect_equal(res$status, 200)
  expect_equal(res$headers$`Content-Type`, "application/octet-stream")
  expect_match(
    res$headers$`Content-Disposition`,
    'attachment; filename="MWI_coarse-output_\\d+-\\d+.zip"')
  ## Size of bytes is close to expected
  size <- length(res$body)
  expect_equal(res$headers$`Content-Length`, size)
  expect_equal(size, file.size(
    system_file("output", "malawi_coarse_output_download.zip")))
})

test_that("summary report download returns bytes", {
  test_mock_model_available()
  q <- test_queue_result()

  ## Submit download request
  submit <- endpoint_download_submit(q$queue)
  submit_response <- submit$run(q$calibrate_id, "summary")

  expect_equal(submit_response$status_code, 200)
  expect_true(!is.null(submit_response$data$id))

  ## Status
  out <- q$queue$queue$task_wait(submit_response$data$id)
  status <- endpoint_download_status(q$queue)
  status_response <- status$run(submit_response$data$id)

  expect_equal(status_response$status_code, 200)
  expect_equal(status_response$data$id, submit_response$data$id)
  expect_true(status_response$data$done)
  expect_equal(status_response$data$status, scalar("COMPLETE"))
  expect_true(status_response$data$success)
  expect_equal(status_response$data$queue, scalar(0))
  expect_length(status_response$data$progress, 1)

  ## Get result
  result <- endpoint_download_result(q$queue)
  response <- result$run(status_response$data$id)
  expect_equal(response$status_code, 200)
  expect_match(
    response$headers$`Content-Disposition`,
    'attachment; filename="MWI_summary-report_\\d+-\\d+.html"')
  size <- length(response$data)
  ## There is some data in the report
  expect_true(size > 10000)
  expect_equal(response$headers$`Content-Length`, size)
})

test_that("api can call summary report download", {
  test_redis_available()
  test_mock_model_available()
  q <- test_queue_result()
  api <- api_build(q$queue)

  ## Submit download request
  submit <- api$request("POST", paste0("/download/submit/summary/",
                                      q$calibrate_id))
  submit_body <- jsonlite::fromJSON(submit$body)
  expect_equal(submit$status, 200)
  expect_true(!is.null(submit_body$data$id))

  ## Status
  out <- q$queue$queue$task_wait(submit_body$data$id)
  status <- api$request("GET",
                        paste0("/download/status/", submit_body$data$id))

  expect_equal(status$status, 200)
  status_body <- jsonlite::fromJSON(status$body)
  expect_equal(status_body$data$id, submit_body$data$id)
  expect_true(status_body$data$done)
  expect_equal(status_body$data$status, "COMPLETE")
  expect_true(status_body$data$success)
  expect_equal(status_body$data$queue, 0)
  expect_length(status_body$data$progress, 1)

  ## Get result
  res <- api$request("GET", paste0("/download/result/", submit_body$data$id))

  expect_equal(res$status, 200)
  expect_equal(res$headers$`Content-Type`, "application/octet-stream")
  expect_match(
    res$headers$`Content-Disposition`,
    'attachment; filename="MWI_summary-report_\\d+-\\d+.html"')
  ## Size of bytes is close to expected
  size <- length(res$body)
  ## There is some data in the report
  expect_true(size > 10000)
  expect_equal(res$headers$`Content-Length`, size)
})

test_that("download returns useful error if model run fails", {
  test_redis_available()
  test_mock_model_available()

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

  ## Run the model
  withr::with_envvar(c("USE_MOCK_MODEL" = "false"), {
    queue <- test_queue(workers = 1)
    model_submit <- submit_model(queue)
    response <- model_submit(readLines(path))
    expect_true("id" %in% names(response))
  })

  out <- queue$queue$task_wait(response$id)
  download <- download_submit(queue)
  error <- expect_error(download(response$id, "spectrum"))
  expect_equal(error$data[[1]]$error, scalar("MODEL_RUN_FAILED"))
  expect_match(error$data[[1]]$detail,
               scalar("Required columns not found: sex"))
  expect_equal(error$status_code, 400)
})

test_that("download returns useful error if model result can't be retrieved", {
  test_redis_available()
  test_mock_model_available()

  ## Try to download with task ID doesn't exist
  queue <- test_queue(workers = 0)
  download <- download_submit(queue)
  error <- expect_error(download("id1", "spectrum"))
  expect_equal(error$data[[1]]$error, scalar("FAILED_TO_RETRIEVE_RESULT"))
  expect_equal(error$data[[1]]$detail, scalar("Failed to fetch result"))
  expect_equal(error$status_code, 400)
})

test_that("download fails with old model run result", {
  test_redis_available()
  test_mock_model_available()

  ## Return v0.1.34 model results
  q <- test_queue_result(model = mock_model_v0.1.38,
                         calibrate = mock_model_v0.1.38)

  endpoint <- endpoint_download_submit(q$queue)
  res <- endpoint$run(q$calibrate_id, "spectrum")
  expect_equal(res$value$status, scalar("failure"))
  expect_equal(res$value$errors[[1]]$error, scalar("SERVER_ERROR"))
  expect_equal(
    res$value$errors[[1]]$detail,
    scalar("Model output out of date please re-run model and try again."))
  expect_equal(res$status_code, 500)
})

test_that("trying to download result for errored model run returns error", {
  queue <- MockQueue$new(workers = 1)
  payload <- setup_payload_submit()
  model_submit <- submit_model(queue)
  response <- model_submit(payload)
  expect_true("id" %in% names(response))
  out <- queue$queue$task_wait(response$id)

  download <- download_result(queue)
  error <- expect_error(download(response$id))

  expect_equal(error$status_code, 400)
  expect_equal(names(error$data[[1]]), c("error", "detail", "key"))
  expect_equal(error$data[[1]]$error, scalar("OUTPUT_GENERATION_FAILED"))
  expect_equal(error$data[[1]]$detail, scalar("test error"))
})

test_that("download result returns formatted error if unexpected issue", {
  queue <- MockQueue$new(workers = 0)
  download <- download_result(queue)
  error <- expect_error(download("1"))

  expect_equal(error$status_code, 400)
  expect_equal(names(error$data[[1]]), c("error", "detail", "key"))
  expect_equal(error$data[[1]]$error, scalar("FAILED_TO_RETRIEVE_RESULT"))
  expect_equal(error$data[[1]]$detail, scalar("Missing result for task: '1'"))
})

test_that("download submit returns error if queueing fails", {
  test_redis_available()
  ## Create mocks
  queue <- test_queue(workers = 0)
  mock_submit_download <- function(res, type, notes, state) {
    stop("Failed to queue")
  }

  ## Call the endpoint
  download <- download_submit(queue)
  mockery::stub(download, "queue$submit_download", mock_submit_download)
  mockery::stub(download, "verify_result_available", TRUE)
  error <- expect_error(download("1", "spectrum"))

  expect_equal(error$data[[1]]$error, scalar("FAILED_TO_QUEUE"))
  expect_equal(error$data[[1]]$detail, scalar("Failed to queue"))
  expect_equal(error$status_code, 400)
})

test_that("download unknown file type returns error", {
  error <- expect_error(download(NULL, "unknown", "path"))
  expect_equal(error$data[[1]]$error, scalar("INVALID_DOWNLOAD_TYPE"))
  expect_equal(error$data[[1]]$detail, scalar(
    "Failed to generate download for unknown type, contact system admin."))
  expect_equal(error$status_code, 400)
})

test_that("trying to get download with invalid result returns error", {
  test_mock_model_available()
  q <- test_queue_result()

  endpoint <- endpoint_download_result(q$queue)
  out <- endpoint$run(q$calibrate_id)
  expect_equal(out$error$data[[1]]$error, scalar("OUTPUT_GENERATION_FAILED"))
  expect_match(out$error$data[[1]]$detail,
               scalar("Failed to generate metadata, output format is invalid"))
  expect_equal(out$status_code, 400)
})

test_that("comparison report download returns bytes", {
  test_mock_model_available()
  q <- test_queue_result()

  ## Submit download request
  submit <- endpoint_download_submit(q$queue)
  submit_response <- submit$run(q$calibrate_id, "comparison")

  expect_equal(submit_response$status_code, 200)
  expect_true(!is.null(submit_response$data$id))

  ## Status
  out <- q$queue$queue$task_wait(submit_response$data$id)
  status <- endpoint_download_status(q$queue)
  status_response <- status$run(submit_response$data$id)

  expect_equal(status_response$status_code, 200)
  expect_equal(status_response$data$id, submit_response$data$id)
  expect_true(status_response$data$done)
  expect_equal(status_response$data$status, scalar("COMPLETE"))
  expect_true(status_response$data$success)
  expect_equal(status_response$data$queue, scalar(0))
  expect_length(status_response$data$progress, 1)

  ## Get result
  result <- endpoint_download_result(q$queue)
  response <- result$run(status_response$data$id)
  expect_equal(response$status_code, 200)
  expect_match(
    response$headers$`Content-Disposition`,
    'attachment; filename="MWI_comparison-report_\\d+-\\d+.html"')
  size <- length(response$data)
  ## There is some data in the report
  expect_true(size > 10000)
  expect_equal(response$headers$`Content-Length`, size)
})

test_that("api can call comparison report download", {
  test_redis_available()
  test_mock_model_available()
  q <- test_queue_result()
  api <- api_build(q$queue)

  ## Submit download request
  submit <- api$request("POST", paste0("/download/submit/comparison/",
                                      q$calibrate_id))
  submit_body <- jsonlite::fromJSON(submit$body)
  expect_equal(submit$status, 200)
  expect_true(!is.null(submit_body$data$id))

  ## Status
  out <- q$queue$queue$task_wait(submit_body$data$id)
  status <- api$request("GET",
                        paste0("/download/status/", submit_body$data$id))

  expect_equal(status$status, 200)
  status_body <- jsonlite::fromJSON(status$body)
  expect_equal(status_body$data$id, submit_body$data$id)
  expect_true(status_body$data$done)
  expect_equal(status_body$data$status, "COMPLETE")
  expect_true(status_body$data$success)
  expect_equal(status_body$data$queue, 0)
  expect_length(status_body$data$progress, 1)

  ## Get result
  res <- api$request("GET", paste0("/download/result/", submit_body$data$id))

  expect_equal(res$status, 200)
  expect_equal(res$headers$`Content-Type`, "application/octet-stream")
  expect_match(
    res$headers$`Content-Disposition`,
    'attachment; filename="MWI_comparison-report_\\d+-\\d+.html"')
  ## Size of bytes is close to expected
  size <- length(res$body)
  ## There is some data in the report
  expect_true(size > 10000)
  expect_equal(res$headers$`Content-Length`, size)
})

test_that("spectrum download can include notes", {
  test_mock_model_available()
  q <- test_queue_result()

  ## Submit download request
  submit <- endpoint_download_submit(q$queue)
  payload <- setup_payload_download_request(include_state = FALSE)
  submit_response <- submit$run(q$calibrate_id, "spectrum", payload)

  expect_equal(submit_response$status_code, 200)
  expect_true(!is.null(submit_response$data$id))

  ## Status
  out <- q$queue$queue$task_wait(submit_response$data$id)
  status <- endpoint_download_status(q$queue)
  status_response <- status$run(submit_response$data$id)

  expect_equal(status_response$status_code, 200)
  expect_equal(status_response$data$id, submit_response$data$id)
  expect_true(status_response$data$done)
  expect_equal(status_response$data$status, scalar("COMPLETE"))
  expect_true(status_response$data$success)
  expect_equal(status_response$data$queue, scalar(0))
  expect_length(status_response$data$progress, 1)

  ## Get result
  result <- endpoint_download_result(q$queue)
  response <- result$run(status_response$data$id)
  expect_equal(response$status_code, 200)
  expect_match(response$headers$`Content-Disposition`,
               'attachment; filename="MWI_naomi-output_\\d+-\\d+.zip"')

  tmp <- tempfile()
  dest <- tempfile()
  writeBin(as.vector(response$data), tmp)
  zip::unzip(tmp, exdir = dest)
  expect_true(NOTES_PATH %in% list.files(dest))
  notes <- readLines(file.path(dest, NOTES_PATH))
  expect_equal(notes, c(
    "Project notes:",
    "",
    "My project 123",
    "2022/05/17 12:34:21",
    "These are my project notes",
    "",
    "Version notes:",
    "",
    "Version 2",
    "2022/05/17 12:34:21",
    "Notes specific to this version",
    "",
    "Version 1",
    "2022/05/14 09:12:54",
    "Notes from the first version",
    ""))
})

test_that("api: spectrum download can include notes", {
  test_redis_available()
  test_mock_model_available()
  q <- test_queue_result()
  api <- api_build(q$queue)

  ## Submit download request
  payload <- setup_payload_download_request(include_state = FALSE)
  submit <- api$request("POST",
                        paste0("/download/submit/spectrum/", q$calibrate_id),
                        body = payload)
  submit_body <- jsonlite::fromJSON(submit$body)
  expect_equal(submit$status, 200)
  expect_true(!is.null(submit_body$data$id))

  ## Status
  out <- q$queue$queue$task_wait(submit_body$data$id)
  status <- api$request("GET",
                        paste0("/download/status/", submit_body$data$id))

  expect_equal(status$status, 200)
  status_body <- jsonlite::fromJSON(status$body)
  expect_equal(status_body$data$id, submit_body$data$id)
  expect_true(status_body$data$done)
  expect_equal(status_body$data$status, "COMPLETE")

  ## Get result
  res <- api$request("GET", paste0("/download/result/", submit_body$data$id))
  expect_equal(res$status, 200)

  tmp <- tempfile()
  dest <- tempfile()
  writeBin(as.vector(res$body), tmp)
  zip::unzip(tmp, exdir = dest)
  expect_true(NOTES_PATH %in% list.files(dest))
  notes <- readLines(file.path(dest, NOTES_PATH))
  expect_equal(notes, c(
    "Project notes:",
    "",
    "My project 123",
    "2022/05/17 12:34:21",
    "These are my project notes",
    "",
    "Version notes:",
    "",
    "Version 2",
    "2022/05/17 12:34:21",
    "Notes specific to this version",
    "",
    "Version 1",
    "2022/05/14 09:12:54",
    "Notes from the first version",
    ""))
})

test_that("sending notes to non-spectrum download doesn't fail", {
  test_mock_model_available()
  q <- test_queue_result()

  ## Submit download request
  submit <- endpoint_download_submit(q$queue)
  payload <- setup_payload_download_request(include_state = FALSE)
  submit_response <- submit$run(q$calibrate_id, "summary", payload)

  expect_equal(submit_response$status_code, 200)
  expect_true(!is.null(submit_response$data$id))

  ## Status
  out <- q$queue$queue$task_wait(submit_response$data$id)
  status <- endpoint_download_status(q$queue)
  status_response <- status$run(submit_response$data$id)

  expect_equal(status_response$status_code, 200)
  expect_equal(status_response$data$id, submit_response$data$id)
  expect_true(status_response$data$done)
  expect_equal(status_response$data$status, scalar("COMPLETE"))
  expect_true(status_response$data$success)
  expect_equal(status_response$data$queue, scalar(0))
  expect_length(status_response$data$progress, 1)

  ## Get result
  result <- endpoint_download_result(q$queue)
  response <- result$run(status_response$data$id)
  expect_equal(response$status_code, 200)
})

test_that("spectrum download can include project state", {
  test_mock_model_available()
  q <- test_queue_result()

  ## Submit download request
  submit <- endpoint_download_submit(q$queue)
  payload <- setup_payload_download_request(include_notes = FALSE)
  submit_response <- submit$run(q$calibrate_id, "spectrum", payload)

  expect_equal(submit_response$status_code, 200)
  expect_true(!is.null(submit_response$data$id))

  ## Status
  out <- q$queue$queue$task_wait(submit_response$data$id)
  status <- endpoint_download_status(q$queue)
  status_response <- status$run(submit_response$data$id)

  expect_equal(status_response$status_code, 200)
  expect_equal(status_response$data$id, submit_response$data$id)
  expect_true(status_response$data$done)
  expect_equal(status_response$data$status, scalar("COMPLETE"))
  expect_true(status_response$data$success)
  expect_equal(status_response$data$queue, scalar(0))
  expect_length(status_response$data$progress, 1)

  ## Get result
  result <- endpoint_download_result(q$queue)
  response <- result$run(status_response$data$id)
  expect_equal(response$status_code, 200)
  expect_match(response$headers$`Content-Disposition`,
               'attachment; filename="MWI_naomi-output_\\d+-\\d+.zip"')

  tmp <- tempfile()
  dest <- tempfile()
  writeBin(as.vector(response$data), tmp)
  zip::unzip(tmp, exdir = dest)
  expect_true(PROJECT_STATE_PATH %in% list.files(dest, recursive = TRUE))
  state <- jsonlite::read_json(file.path(dest, PROJECT_STATE_PATH),
                               simplifyVector = FALSE)
  expect_equal(state, jsonlite::fromJSON(setup_payload_project_state(),
                                          simplifyVector = FALSE))
})

test_that("api: spectrum download can include project state", {
  test_redis_available()
  test_mock_model_available()
  q <- test_queue_result()
  api <- api_build(q$queue)

  ## Submit download request
  payload <- setup_payload_download_request(include_notes = FALSE)
  submit <- api$request("POST",
                        paste0("/download/submit/spectrum/", q$calibrate_id),
                        body = payload)
  submit_body <- jsonlite::fromJSON(submit$body)
  expect_equal(submit$status, 200)
  expect_true(!is.null(submit_body$data$id))

  ## Status
  out <- q$queue$queue$task_wait(submit_body$data$id)
  status <- api$request("GET",
                        paste0("/download/status/", submit_body$data$id))

  expect_equal(status$status, 200)
  status_body <- jsonlite::fromJSON(status$body)
  expect_equal(status_body$data$id, submit_body$data$id)
  expect_true(status_body$data$done)
  expect_equal(status_body$data$status, "COMPLETE")

  ## Get result
  res <- api$request("GET", paste0("/download/result/", submit_body$data$id))
  expect_equal(res$status, 200)

  tmp <- tempfile()
  dest <- tempfile()
  writeBin(as.vector(res$body), tmp)
  zip::unzip(tmp, exdir = dest)
  expect_true(PROJECT_STATE_PATH %in% list.files(dest, recursive = TRUE))
  state <- jsonlite::read_json(file.path(dest, PROJECT_STATE_PATH),
                               simplifyVector = FALSE)
  expect_equal(state, jsonlite::fromJSON(setup_payload_project_state(),
                                         simplifyVector = FALSE))
})

test_that("api: spectrum download can include notes and state", {
  test_redis_available()
  test_mock_model_available()
  q <- test_queue_result()
  api <- api_build(q$queue)

  ## Submit download request
  payload <- setup_payload_download_request()
  submit <- api$request("POST",
                        paste0("/download/submit/spectrum/", q$calibrate_id),
                        body = payload)
  submit_body <- jsonlite::fromJSON(submit$body)
  expect_equal(submit$status, 200)
  expect_true(!is.null(submit_body$data$id))

  ## Status
  out <- q$queue$queue$task_wait(submit_body$data$id)
  status <- api$request("GET",
                        paste0("/download/status/", submit_body$data$id))

  expect_equal(status$status, 200)
  status_body <- jsonlite::fromJSON(status$body)
  expect_equal(status_body$data$id, submit_body$data$id)
  expect_true(status_body$data$done)
  expect_equal(status_body$data$status, "COMPLETE")

  ## Get result
  res <- api$request("GET", paste0("/download/result/", submit_body$data$id))
  expect_equal(res$status, 200)

  tmp <- tempfile()
  dest <- tempfile()
  writeBin(as.vector(res$body), tmp)
  zip::unzip(tmp, exdir = dest)
  expect_true(NOTES_PATH %in% list.files(dest))
  notes <- readLines(file.path(dest, NOTES_PATH))
  expect_equal(notes, c(
    "Project notes:",
    "",
    "My project 123",
    "2022/05/17 12:34:21",
    "These are my project notes",
    "",
    "Version notes:",
    "",
    "Version 2",
    "2022/05/17 12:34:21",
    "Notes specific to this version",
    "",
    "Version 1",
    "2022/05/14 09:12:54",
    "Notes from the first version",
    ""))

  expect_true(PROJECT_STATE_PATH %in% list.files(dest, recursive = TRUE))
  state <- jsonlite::read_json(file.path(dest, PROJECT_STATE_PATH),
                               simplifyVector = FALSE)
  expect_equal(state, jsonlite::fromJSON(setup_payload_project_state(),
                                         simplifyVector = FALSE))
})

test_that("api: programme and anc files are optional in spectrum download", {
  test_redis_available()
  test_mock_model_available()
  q <- test_queue_result()
  api <- api_build(q$queue)

  ## Prepare body without anc or programme
  payload <- setup_payload_download_request(include_notes = FALSE)
  json <- jsonlite::fromJSON(payload, simplifyVector = FALSE)
  json$state$datasets$programme <- NULL
  json$state$datasets$anc <- NULL
  t <- tempfile()
  jsonlite::write_json(json, t, auto_unbox = TRUE)

  ## Submit download request
  submit <- api$request("POST",
                        paste0("/download/submit/spectrum/", q$calibrate_id),
                        body = readLines(t))
  submit_body <- jsonlite::fromJSON(submit$body)
  expect_equal(submit$status, 200)
  expect_true(!is.null(submit_body$data$id))

  ## Status
  out <- q$queue$queue$task_wait(submit_body$data$id)
  status <- api$request("GET",
                        paste0("/download/status/", submit_body$data$id))

  expect_equal(status$status, 200)
  status_body <- jsonlite::fromJSON(status$body)
  expect_equal(status_body$data$id, submit_body$data$id)
  expect_true(status_body$data$done)
  expect_equal(status_body$data$status, "COMPLETE")

  ## Get result
  res <- api$request("GET", paste0("/download/result/", submit_body$data$id))
  expect_equal(res$status, 200)

  tmp <- tempfile()
  dest <- tempfile()
  writeBin(as.vector(res$body), tmp)
  zip::unzip(tmp, exdir = dest)
  expect_true(PROJECT_STATE_PATH %in% list.files(dest, recursive = TRUE))
  state <- jsonlite::read_json(file.path(dest, PROJECT_STATE_PATH),
                               simplifyVector = FALSE)
  expect_setequal(names(state$datasets),
                  c("pjnz", "population", "shape", "survey"))
})

test_that("spectrum download is translated", {
  test_mock_model_available()
  q <- test_queue_result()

  response <- with_hintr_language("fr", {
    submit <- endpoint_download_submit(q$queue)
    submit_response <- submit$run(q$calibrate_id, "spectrum")
  })

  expect_equal(submit_response$status_code, 200)
  expect_true(!is.null(submit_response$data$id))

  ## Status
  out <- q$queue$queue$task_wait(submit_response$data$id)
  status <- endpoint_download_status(q$queue)
  status_response <- status$run(submit_response$data$id)

  expect_equal(status_response$status_code, 200)
  expect_equal(status_response$data$id, submit_response$data$id)
  expect_true(status_response$data$done)
  expect_equal(status_response$data$status, scalar("COMPLETE"))
  expect_true(status_response$data$success)
  expect_equal(status_response$data$queue, scalar(0))
  expect_length(status_response$data$progress, 1)

  ## Get result
  result <- endpoint_download_result(q$queue)
  response <- result$run(status_response$data$id)
  expect_equal(response$status_code, 200)
  expect_match(response$headers$`Content-Disposition`,
               'attachment; filename="MWI_naomi-output_\\d+-\\d+.zip"')
  size <- length(response$data)
  expect_equal(response$headers$`Content-Length`, size)

  t <- tempfile()
  dest <- tempfile()
  writeBin(as.vector(response$data), t)
  zip::unzip(t, exdir = dest)
  expect_true(INDICATORS_PATH %in% list.files(dest))
  indicators <- read.csv(file.path(dest, INDICATORS_PATH))
  expect_true("Prévalence du VIH" %in% unique(indicators$indicator_label))
})

test_that("comparison report download errors for old model output", {
  test_mock_model_available()
  q <- test_queue_result(model = mock_model_v1.0.7,
                         calibrate = mock_calibrate_v1.0.7)

  ## Submit download request
  submit <- endpoint_download_submit(q$queue)
  submit_response <- submit$run(q$calibrate_id, "comparison")

  expect_equal(submit_response$status_code, 500)
  expect_equal(submit_response$value$errors[[1]]$detail, scalar(
    "Model output out of date please re-run model and try again."))
})
