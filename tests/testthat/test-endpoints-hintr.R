test_that("endpoint worker status works", {
  test_redis_available()
  queue <- test_queue(workers = 2)
  status <- worker_status(queue)

  response <- status()
  expect_equal(unlist(response, FALSE, FALSE), rep("IDLE", 2))

  rrq::rrq_worker_stop(timeout = 5, controller = queue$controller)
  Sys.sleep(5)
  response <- status()
  expect_equal(unlist(response, FALSE, FALSE), rep("EXITED", 2))

  rrq::rrq_worker_delete_exited(controller = queue$controller)
  response <- status()
  expect_equal(response, setNames(list(), character()))
})

test_that("stop calls quit and stop_workers", {
  test_redis_available()
  queue <- test_queue(workers = 0)
  mock_worker_stop <- mockery::mock()
  mock_quit <- mockery::mock()

  with_mocked_bindings(
    {
      endpoint <- hintr_stop(queue)
      mockery::stub(endpoint, "quit", mock_quit)
      endpoint()
    },
    "worker_stop" = mock_worker_stop,
  )

  ## Quit call:
  mockery::expect_called(mock_quit, 1)
  expect_equal(mockery::mock_calls(mock_quit)[[1]], quote(quit(save = "no")))

  ## Worker stop:
  mockery::expect_called(mock_worker_stop, 1)
})

test_that("starting API clears any exited workers", {
  test_redis_available()
  queue_id <- "test-id"
  queue <- Queue$new(queue_id, workers = 0)
  status <- worker_status(queue)

  api <- api(queue_id, workers = 2)
  rrq::rrq_worker_stop(timeout = 5, controller = queue$controller)
  Sys.sleep(5)
  response <- status()
  expect_equal(unlist(response, FALSE, FALSE), rep("EXITED", 2))

  api <- api(queue_id, workers = 0)
  response <- status()
  expect_equal(response, setNames(list(), character()))
})
