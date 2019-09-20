context("queue")

test_that("queue works as intended", {
  test_redis_available()

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
  job_id <- queue$submit(NULL, list(sleep = 1))
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
  Sys.sleep(2)
  status <- queue$status(job_id)
  expect_equal(status$status, "COMPLETE")
  expect_true(status$done)
  expect_true(status$success)
  expect_equal(status$queue, 0)

  ## Result can be retrieved after task has completed
  res <- queue$result(job_id)
  expect_equal(names(res), c("data", "filters"))
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
