context("main")

test_that("main_api_args", {
  expect_equal(main_api_args(c()),
               list(port = 8888, queue_id = NULL, workers = 2))
  expect_equal(
    main_api_args(c("--workers=0", "--port", "80", "hintr")),
    list(port = 80, queue_id = "hintr", workers = 0))
})

test_that("main_worker_args", {
  expect_equal(main_worker_args(c()), list(queue_id = NULL))
  expect_equal(main_worker_args("hintr"), list(queue_id = "hintr"))
})
