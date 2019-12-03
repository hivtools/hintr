context("api")

test_that("don't change language if not asked to", {
  data <- new.env()
  tr <- traduire::translator("package:hintr")
  expect_equal(tr$language(), "en")

  res <- MockPlumberResponse$new()
  req <- list()
  value <- list("the response")

  expect_null(api_set_language(data, req, res))
  expect_equal(tr$language(), "en")
  expect_identical(api_reset_language(data, req, res, value), value)
  expect_equal(tr$language(), "en")
})

test_that("change language based on header", {
  data <- new.env()
  tr <- traduire::translator("package:hintr")
  expect_equal(tr$language(), "en")

  res <- MockPlumberResponse$new()
  req <- list(HEADERS = c("accept-language" = "fr"))
  value <- list("the response")

  expect_null(api_set_language(data, req, res))
  expect_equal(tr$language(), "fr")
  expect_identical(api_reset_language(data, req, res, value), value)
  expect_equal(tr$language(), "en")
})
