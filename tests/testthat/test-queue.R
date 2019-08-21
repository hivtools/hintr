context("queue")

test_that("queue works as intended", {
  test_redis_available()

  ## queue can be started ensuring starting with a clean queue
  if (!is.null(global_queue$queue)){
    model_queue_stop()
  }
  queue <- model_queue_start(tempfile())
  expect_equal(queue$worker_len(), 2)

  worker_1 <- queue$worker_list()[[1]]
  worker_2 <- queue$worker_list()[[2]]

  expect_equal(nrow(queue$worker_log_tail(worker_1)), 1)
  expect_equal(queue$worker_log_tail(worker_1)$command, "ALIVE")

  expect_equal(nrow(queue$worker_log_tail(worker_2)), 1)
  expect_equal(queue$worker_log_tail(worker_2)$command, "ALIVE")

  expect_length(queue$task_list(), 0)

  ## jobs can be pushed to queue
  job_id <- model_queue_submit(list(), list())
  expect_length(queue$task_list(), 1)

  ## status can be retireved
  ## sleep for 5s to ensure job has been picked up by runner otherwise
  ## will be pending
  Sys.sleep(5)
  status <- model_queue_status(job_id)
  expect_equal(status$status, "RUNNING")
  expect_false(status$done)
  expect_null(status$success)
  expect_equal(status$queue, 0)

  ## After task has completed
  Sys.sleep(10)
  status <- model_queue_status(job_id)
  expect_equal(status$status, "COMPLETE")
  expect_true(status$done)
  expect_true(status$success)
  expect_equal(status$queue, 0)

  ## Result can be retrieved after task has completed
  res <- model_queue_result(job_id)
  expect_equal(res, 2)
  expect_length(queue$task_list(), 1)

  ## task can be cleaned up
  model_queue_remove(job_id)
  expect_length(queue$task_list(), 0)

  ## workers can be stopped
  model_queue_finalize(queue)
  ## Pase for workers to be stopped
  Sys.sleep(1)
  expect_equal(queue$worker_len(), 0)
  expect_equal(queue$worker_log_tail(worker_1)$command, "STOP")
  expect_equal(queue$worker_log_tail(worker_2)$command, "STOP")
  expect_equal(global_queue$queue, queue)

  ## queue can be torn down
  model_queue_stop()
  expect_null(global_queue$queue)
})
