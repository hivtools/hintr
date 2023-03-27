test_that("can return upload metadata for ADR spectrum", {
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

  ## Get metadata
  metadata <- endpoint_adr_metadata(q$queue)
  res <- metadata$run(submit_response$data$id)
  expect_equal(res$status_code, 200)
  expect_equal(names(res$data), c("type", "description"))
  expect_equal(res$data$type, scalar("spectrum"))
  expect_s3_class(res$data$description, c("scalar", "character"))
})

test_that("can return upload metadata for ADR coarse-output", {
  test_mock_model_available()
  q <- test_queue_result()

  ## Submit download request
  submit <- endpoint_download_submit(q$queue)
  submit_response <- submit$run(q$calibrate_id, "coarse-output")

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

  ## Get metadata
  metadata <- endpoint_adr_metadata(q$queue)
  res <- metadata$run(submit_response$data$id)
  expect_equal(res$status_code, 200)
  expect_equal(names(res$data), c("type", "description"))
  expect_equal(res$data$type, scalar("coarse_output"))
  expect_null(res$data$description)
})

test_that("can return upload metadata for ADR summary report", {
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

  ## Get metadata
  metadata <- endpoint_adr_metadata(q$queue)
  res <- metadata$run(submit_response$data$id)
  expect_equal(res$status_code, 200)
  expect_equal(names(res$data), c("type", "description"))
  expect_equal(res$data$type, scalar("summary"))
  expect_s3_class(res$data$description, c("scalar", "character"))
})

test_that("can return upload metadata for comparison report", {
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

  ## Get metadata
  metadata <- endpoint_adr_metadata(q$queue)
  res <- metadata$run(submit_response$data$id)
  expect_equal(res$status_code, 200)
  expect_equal(names(res$data), c("type", "description"))
  expect_equal(res$data$type, scalar("comparison"))
  expect_s3_class(res$data$description, c("scalar", "character"))
})

test_that("trying to get ADR metadata without result available throws error", {
  test_mock_model_available()
  q <- test_queue_result()

  metadata <- endpoint_adr_metadata(q$queue)
  out <- metadata$run("some_id")
  expect_equal(out$error$data[[1]]$error, scalar("FAILED_TO_RETRIEVE_RESULT"))
  expect_match(out$error$data[[1]]$detail,
               scalar("Missing result for task: 'some_id'"))
  expect_equal(out$status_code, 400)

  out <- metadata$run(q$calibrate_id)
  expect_equal(out$error$data[[1]]$error, scalar("OUTPUT_GENERATION_FAILED"))
  expect_match(out$error$data[[1]]$detail,
               scalar("Failed to generate metadata, output format is invalid"))
  expect_equal(out$status_code, 400)
})
