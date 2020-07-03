context("endpoints-hintr")

test_that("endpoint hintr works", {
  response <- endpoint_hintr_version()
  response <- jsonlite::parse_json(response)

  expect_is(response$data, "list")
  expect_setequal(names(response$data), c("hintr", "naomi", "rrq", "traduire"))
  expect_equal(response$data$rrq, as.character(packageVersion("rrq")))
})

test_that("endpoint worker status works", {
  test_redis_available()
  queue <- test_queue()
  endpoint <- endpoint_hintr_worker_status(queue)

  response <- jsonlite::parse_json(endpoint())
  expect_equal(unlist(response$data, FALSE, FALSE), rep("IDLE", 2))

  queue$queue$worker_stop(timeout = 5)
  Sys.sleep(5)
  response <- jsonlite::parse_json(endpoint())
  expect_equal(unlist(response$data, FALSE, FALSE), rep("EXITED", 2))

  queue$queue$worker_delete_exited()
  response <- jsonlite::parse_json(endpoint())
  expect_equal(response$data, setNames(list(), character()))
})

test_that("stop calls quit and stop_workers", {
  test_redis_available()
  queue <- test_queue(workers = 0)
  unlockBinding("worker_stop", queue$queue)
  queue$queue$worker_stop <- mockery::mock()
  mock_quit <- mockery::mock()
  endpoint <- endpoint_hintr_stop(queue)
  mockery::stub(endpoint, "quit", mock_quit)
  endpoint(NULL, NULL)

  ## Quit call:
  mockery::expect_called(mock_quit, 1)
  expect_equal(mockery::mock_calls(mock_quit)[[1]], quote(quit(save = "no")))

  ## Worker stop:
  mockery::expect_called(queue$queue$worker_stop, 1)
  expect_equal(length(mockery::mock_calls(queue$queue$worker_stop)[[1]]), 1)
})
