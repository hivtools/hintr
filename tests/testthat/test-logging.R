context("logging")

test_that("logging produces a message", {
  expect_message(
    api_log("%s %s", "a", "b"),
    "\\[[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}\\] a b")
  expect_message(
    api_log("x"),
    "\\[[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}\\] x")
})

test_that("log start includes request and path info", {
  req <- list(REQUEST_METHOD = "GET", PATH_INFO = "/my/path")
  expect_message(
    api_log_start(NULL, req, NULL),
    "\\[.+\\] GET /my/path")
})

test_that("log end includes code and response size", {
  value <- list(status = 200, body = "a string")
  expect_message(
    res <- api_log_end(NULL, NULL, NULL, value),
    "\\[.+\\] `--> 200 \\(8 bytes\\)")
  expect_identical(res, value)
})

test_that("log end includes code and response size for binary data", {
  value <- list(status = 200, body = as.raw(sample(256) - 1))
  expect_message(
    res <- api_log_end(NULL, NULL, NULL, value),
    "\\[.+\\] `--> 200 \\(256 bytes\\)")
  expect_identical(res, value)
})
