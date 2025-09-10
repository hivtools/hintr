test_that("main_api_args", {
  default <- main_api_args(c())
  expect_equal(default$port, 8888)
  expect_null(default$queue_id)
  expect_equal(default$workers, 2)
  expect_true(file.exists(default$results_dir))
  expect_equal(default$health_check_interval, 0)
  expect_equal(
    main_api_args(c("--workers=0", "--port", "80", "--results-dir=out",
                    "--prerun-dir=pr", "--inputs-dir=in",
                    "--health-check-interval=20", "hintr")),
    list(port = 80, queue_id = "hintr", workers = 0, results_dir = "out",
         inputs_dir = "in", health_check_interval = 20))
})

test_that("main_worker_args", {
  expect_equal(main_worker_args(c()),
               list(queue_id = NULL, worker_config = "localhost"))
  expect_equal(main_worker_args("--worker-config=localhost"),
               list(queue_id = NULL, worker_config = "localhost"))
  expect_equal(main_worker_args(c("--worker-config=localhost", "hintr")),
               list(queue_id = "hintr", worker_config = "localhost"))
})

test_that("main worker creates worker with multiple queues", {
  mock_rrq_worker <- mockery::mock(list(loop = function() TRUE, cycle = TRUE))
  with_mocked_bindings(
    worker <- main_worker(c("--worker-config=localhost", "queue_id")),
    rrq_worker_new = mock_rrq_worker
  )
  args <- mockery::mock_args(mock_rrq_worker)[[1]]
  expect_equal(args[[1]], "queue_id")
  expect_equal(args$name_config, "localhost")
})

test_that("main worker can create a calibrate only worker", {
  mock_rrq_worker <- mockery::mock(list(loop = function() TRUE, cycle = TRUE))
  with_mocked_bindings(
    worker <- main_worker(c("--worker-config=calibrate_only", "queue_id")),
    rrq_worker_new = mock_rrq_worker
  )
  args <- mockery::mock_args(mock_rrq_worker)[[1]]
  expect_equal(args[[1]], "queue_id")
  expect_equal(args$name_config, "calibrate_only")
})

test_that("main worker single job can create a fit only worker", {
  mock_rrq_worker <- mockery::mock(
    list(step = function(immediate) TRUE, cycle = TRUE))
  with_mocked_bindings(
    worker <- main_worker_single_job(c("--worker-config=fit_only", "queue_id")),
    rrq_worker_new = mock_rrq_worker
  )
  args <- mockery::mock_args(mock_rrq_worker)[[1]]
  expect_equal(args[[1]], "queue_id")
  expect_equal(args$name_config, "fit_only")
})

test_that("can run a single job using worker", {
  test_redis_available()
  test_mock_model_available()

  queue_id <- hintr_queue_id(NULL)
  ## Don't delete data here as we are creating a worker separately which is
  ## leading to some race condition on cleanup. Where it is trying to finalize
  ## the worker after all redis data has been deleted terminating the R process.
  queue <- test_queue(queue_id, workers = 0, delete_data_on_exit = FALSE)
  run_endpoint <- endpoint_model_submit(queue)
  payload <- setup_payload_submit()
  run_response <- run_endpoint$run(payload)

  expect_equal(run_response$status_code, 200)

  expect_equal(queue$status(run_response$data$id)$status, "PENDING")
  msg <- capture_messages(res <- worker_single_job(queue_id, "localhost"))

  expect_true(res)
  expect_equal(queue$status(run_response$data$id)$status, "COMPLETE")
})
