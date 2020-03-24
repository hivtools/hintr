context("queue")

test_that("queue works as intended", {
  test_redis_available()
  test_mock_model_available()

  queue <- Queue$new()
  expect_equal(queue$queue$worker_len(), 2)

  worker_1 <- queue$queue$worker_list()[[1]]
  worker_2 <- queue$queue$worker_list()[[2]]

  expect_equal(nrow(queue$queue$worker_log_tail(worker_1)), 1)
  expect_equal(queue$queue$worker_log_tail(worker_1)$command, "ALIVE")

  expect_equal(nrow(queue$queue$worker_log_tail(worker_2)), 1)
  expect_equal(queue$queue$worker_log_tail(worker_2)$command, "ALIVE")

  expect_length(queue$queue$task_list(), 0)

  ## jobs can be pushed to queue
  job_id <- queue$submit(NULL, list())
  expect_length(queue$queue$task_list(), 1)

  ## status can be retireved
  ## sleep for 5s to ensure job has been picked up by runner otherwise
  ## will be pending
  Sys.sleep(0.1)
  status <- queue$status(job_id)
  expect_equal(status$status, "RUNNING")
  expect_false(status$done)
  expect_equal(status$success, json_verbatim("null"))
  expect_equal(status$queue, 0)

  ## After task has completed
  result <- queue$queue$task_wait(job_id)
  status <- queue$status(job_id)
  expect_equal(status$status, "COMPLETE")
  expect_true(status$done)
  expect_true(status$success)
  expect_equal(status$queue, 0)

  ## Result can be retrieved after task has completed
  res <- queue$result(job_id)
  expect_equal(names(res),
               c("output_path", "spectrum_path", "summary_path", "metadata"))
  expect_length(queue$queue$task_list(), 1)

  ## task can be cleaned up
  queue$remove(job_id)
  expect_length(queue$queue$task_list(), 0)

  con <- queue$queue$con
  key <- queue$queue$keys$worker_name
  expect_equal(con$SCARD(key), 2)

  rm(queue)
  gc()

  expect_equal(con$SCARD(key), 0)
})

test_that("queue_id is generated if not supplied", {
  withr::with_envvar(
    c("HINTR_QUEUE_ID" = NA),
    expect_match(hintr_queue_id(NULL), "^hintr:[[:xdigit:]]+$"))
  withr::with_envvar(
    c("HINTR_QUEUE_ID" = "myqueue"),
    expect_equal(hintr_queue_id(NULL), "myqueue"))
})


test_that("queue_id is required for workers", {
  withr::with_envvar(
    c("HINTR_QUEUE_ID" = NA),
    expect_error(hintr_queue_id(NULL, TRUE),
                 "Environment variable 'HINTR_QUEUE_ID' is not set"))
})

test_that("queue_id is returned if supplied", {
  withr::with_envvar(
    c("HINTR_QUEUE_ID" = NA),
    expect_equal(hintr_queue_id("myqueue", TRUE), "myqueue"))
})
