test_that("queue works as intended", {
  test_redis_available()
  test_mock_model_available()

  queue <- Queue$new(timeout = 300)
  ctrl <- queue$controller
  expect_equal(rrq::rrq_worker_len(controller = ctrl), 2)

  worker_1 <- rrq::rrq_worker_list(controller = ctrl)[[1]]
  worker_2 <- rrq::rrq_worker_list(controller = ctrl)[[2]]

  expect_equal(
    rrq::rrq_worker_log_tail(worker_1, 5, controller = ctrl)[1, "command"],
    "ALIVE"
  )
  expect_equal(
    rrq::rrq_worker_log_tail(worker_1, 5, controller = ctrl)[5, "message"],
    "TIMEOUT_SET"
  )

  expect_equal(
    rrq::rrq_worker_log_tail(worker_2, 5, controller = ctrl)[1, "command"],
    "ALIVE"
  )
  expect_equal(
    rrq::rrq_worker_log_tail(worker_2, 5, controller = ctrl)[5, "message"],
    "TIMEOUT_SET"
  )

  expect_length(rrq::rrq_task_list(controller = ctrl), 0)

  ## model run can be pushed to queue
  job_id <- queue$submit_model_run(NULL, list())
  expect_length(rrq::rrq_task_list(controller = ctrl), 1)

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
  result <- rrq::rrq_task_wait(job_id, controller = ctrl)
  status <- queue$status(job_id)
  expect_equal(status$status, "COMPLETE")
  expect_true(status$done)
  expect_true(status$success)
  expect_equal(status$queue, 0)

  ## Result can be retrieved after task has completed
  res <- queue$result(job_id)
  expect_equal(names(res),
               c("plot_data_path", "model_output_path", "version", "warnings"))
  expect_length(rrq::rrq_task_list(controller = ctrl), 1)

  ## task can be cleaned up
  queue$remove(job_id)
  expect_length(rrq::rrq_task_list(controller = ctrl), 0)

  con <- ctrl$con
  key <- queue$controller$keys$worker_id
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

test_that("test queue starts workers with timeout", {
  queue <- test_queue(workers = 2)
  timeout <- rrq::rrq_message_send_and_wait(
    "TIMEOUT_GET",
    controller = queue$controller)
  expect_length(timeout, 2)
  expect_equal(timeout[[1]][["timeout_idle"]], 300.0)
  expect_equal(timeout[[2]][["timeout_idle"]], 300.0)
})

test_that("queue starts up normally without a timeout", {
  queue <- Queue$new(workers = 1)
  on.exit(queue$cleanup())
  timeout <- rrq::rrq_message_send_and_wait(
    "TIMEOUT_GET",
    controller = queue$controller,
    progress = FALSE)
  expect_equal(timeout[[1]], c("timeout_idle" = Inf, remaining = Inf))
})

test_that("queue object starts up 2 queues", {
  queue <- test_queue(workers = 2)
  expect_equal(rrq::rrq_worker_config_read("localhost",
                                           controller = queue$controller)$queue,
               c(QUEUE_CALIBRATE, QUEUE_RUN, "default"))
})

test_that("calibrate gets run before model running", {
  queue <- test_queue(workers = 0)
  ctrl <- queue$controller
  worker <- create_blocking_worker(queue$controller)
  run_id <- queue$submit_model_run(NULL, NULL)
  ## Calibrate tasks will error but that is fine - we want to test here
  ## that calibrate & model run get queued and run in the correct order
  calibrate_id <- queue$submit_calibrate(NULL, NULL)

  expect_equal(rrq::rrq_task_status(c(run_id, calibrate_id), controller = ctrl),
               rep("PENDING", 2))
  expect_equal(
    rrq::rrq_queue_list(QUEUE_RUN, controller = ctrl),
    run_id
  )
  expect_equal(
    rrq::rrq_queue_list(QUEUE_CALIBRATE, controller = ctrl),
    calibrate_id
  )
  worker$step(TRUE)
  expect_equal(rrq::rrq_task_status(c(run_id, calibrate_id), controller = ctrl),
               c("PENDING", "ERROR"))
  worker$step(TRUE)
  expect_equal(rrq::rrq_task_status(c(run_id, calibrate_id), controller = ctrl),
               c("COMPLETE", "ERROR"))
})

test_that("queue has handle on uploads dir", {
  queue <- Queue$new(workers = 0, inputs_dir = tempdir())
  expect_equal(queue$inputs_dir, tempdir())
})
