context("main")

test_that("main_api_args", {
  default <- main_api_args(c())
  expect_equal(default$port, 8888)
  expect_null(default$queue_id)
  expect_equal(default$workers, 2)
  expect_true(file.exists(default$results_dir))
  expect_true(file.exists(default$prerun_dir))
  expect_equal(
    main_api_args(c("--workers=0", "--port", "80", "--results-dir=out",
                    "--prerun-dir=pr", "hintr")),
    list(port = 80, queue_id = "hintr", workers = 0, results_dir = "out",
         prerun_dir = "pr"))
})

test_that("main_worker_args", {
  expect_equal(main_worker_args(c()), list(queue_id = NULL))
  expect_equal(main_worker_args("hintr"), list(queue_id = "hintr"))
})
