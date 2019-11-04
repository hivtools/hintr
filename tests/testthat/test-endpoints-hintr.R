context("endpoints-hintr")

test_that("endpoint hintr works", {
  response <- endpoint_hintr_version()
  response <- jsonlite::parse_json(response)

  expect_is(response$data, "list")
  expect_setequal(names(response$data), c("hintr", "naomi", "rrq"))
  expect_equal(response$data$rrq, as.character(packageVersion("rrq")))
})

test_that("endpoint sleep works", {
  response <- endpoint_hintr_sleep(NULL, NULL, "0.1")
  response <- jsonlite::parse_json(response)

  expect_is(response$data, "list")
  expect_setequal(names(response$data), c("hintr", "naomi", "rrq"))
  expect_equal(response$data$rrq, as.character(packageVersion("rrq")))
})

test_that("endpoint worker status works", {
  test_redis_available()
  queue <- Queue$new()
  endpoint <- endpoint_hintr_worker_status(queue)

  response <- jsonlite::parse_json(endpoint())
  expect_equal(unlist(response$data, FALSE, FALSE), rep("IDLE", 2))

  queue$queue$worker_stop(timeout = 5)
  response <- jsonlite::parse_json(endpoint())
  expect_equal(unlist(response$data, FALSE, FALSE), rep("EXITED", 2))

  queue$queue$worker_delete_exited()
  response <- jsonlite::parse_json(endpoint())
  expect_equal(response$data, setNames(list(), character()))
})
