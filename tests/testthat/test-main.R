test_that("main_api_args", {
  default <- main_api_args(c())
  expect_equal(default$port, 8888)
  expect_null(default$queue_id)
  expect_equal(default$workers, 2)
  expect_true(file.exists(default$results_dir))
  expect_equal(
    main_api_args(c("--workers=0", "--port", "80", "--results-dir=out",
                    "--prerun-dir=pr", "--inputs-dir=in", "hintr")),
    list(port = 80, queue_id = "hintr", workers = 0, results_dir = "out",
         inputs_dir = "in"))
})

test_that("main_worker_args", {
  expect_equal(main_worker_args(c()),
               list(queue_id = NULL, calibrate_only = FALSE))
  expect_equal(main_worker_args("hintr"),
               list(queue_id = "hintr", calibrate_only = FALSE))
  expect_equal(main_worker_args(c("--calibrate-only")),
               list(queue_id = NULL, calibrate_only = TRUE))
})

test_that("main worker creates worker with multiple queues", {
  mock_rrq_worker <- mockery::mock(list(loop = function() TRUE, cycle = TRUE))
  with_mock(rrq_worker_new = mock_rrq_worker, {
    worker <- main_worker("queue_id")
  })
  args <- mockery::mock_args(mock_rrq_worker)[[1]]
  expect_equal(args[[1]], "queue_id")
  expect_equal(args$name_config, "localhost")
})

test_that("main worker can create a calibrate only worker", {
  mock_rrq_worker <- mockery::mock(list(loop = function() TRUE, cycle = TRUE))
  with_mock(rrq_worker_new = mock_rrq_worker, {
    worker <- main_worker(c("--calibrate-only", "queue_id"))
  })
  args <- mockery::mock_args(mock_rrq_worker)[[1]]
  expect_equal(args[[1]], "queue_id")
  expect_equal(args$name_config, "calibrate_only")
})

