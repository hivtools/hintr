test_that("rehydrate returns json", {
  test_mock_model_available()
  q <- test_queue_result()

  ## Submit rehydrate request
  payload <- setup_reydrate_payload()
  submit <- endpoint_rehydrate_submit(q$queue)
  submit_response <- submit$run(payload)

  expect_equal(submit_response$status_code, 200)
  expect_true(!is.null(submit_response$data$id))

  ## Status
  out <- q$queue$queue$task_wait(submit_response$data$id)
  status <- endpoint_rehydrate_status(q$queue)
  status_response <- status$run(submit_response$data$id)

  expect_equal(status_response$status_code, 200)
  expect_equal(status_response$data$id, submit_response$data$id)
  expect_true(status_response$data$done)
  expect_equal(status_response$data$status, scalar("COMPLETE"))
  expect_true(status_response$data$success)
  expect_equal(status_response$data$queue, scalar(0))
  expect_length(status_response$data$progress, 1)

  ## Get result
  result <- endount_rehydrate_result(q$queue)
  response <- result$run(status_response$data$id)
  expect_equal(response$status_code, 200)
  expect_equal(response$data, readLines("....some json"))
})

test_that("api can call spectrum download", {
  test_redis_available()
  test_mock_model_available()
  q <- test_queue_result()
  api <- api_build(q$queue)

  ## Submit rehydrate request
  payload <- setup_reydrate_payload()
  submit <- api$request("GET",
                        paste0("/rehydrate/submit/spectrum/", q$calibrate_id),
                        body = payload)
  submit_body <- jsonlite::fromJSON(submit$body)
  expect_equal(submit$status, 200)
  expect_true(!is.null(submit_body$data$id))

  ## Status
  out <- q$queue$queue$task_wait(submit_body$data$id)
  status <- api$request("GET",
                        paste0("/rehydrate/status/", submit_body$data$id))

  expect_equal(status$status, 200)
  status_body <- jsonlite::fromJSON(status$body)
  expect_equal(status_body$data$id, submit_body$data$id)
  expect_true(status_body$data$done)
  expect_equal(status_body$data$status, "COMPLETE")
  expect_true(status_body$data$success)
  expect_equal(status_body$data$queue, 0)
  expect_length(status_body$data$progress, 1)

  ## Get result
  res <- api$request("GET", paste0("/rehydrate/result/", submit_body$data$id))

  expect_equal(res$status, 200)
  expect_equal(response$data, readLines("....some json"))
})

test_that("error case 1 - failed to submit", {
  test_mock_model_available()
  q <- test_queue_result()

  ## Submit rehydrate request
  payload <- c("{", '"path": "testdata/Malawi2019.PJNZ"', "}")
  submit <- endpoint_rehydrate_submit(q$queue)
  submit_response <- submit$run(payload)

  expect_equal(submit_response$status_code, 400)
  ## errorr
})

test_that("error case 2 - fail to retrieve result", {
  test_mock_model_available()
  q <- test_queue_result()

  result <- endount_rehydrate_result(q$queue)
  response <- result$run(status_response$data$id)
  ## Errorr
})
