context("api")

test_that("don't change language if not asked to", {
  data <- new.env()
  tr <- hintr_translator()
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

  tr_hintr <- hintr_translator()
  tr_hintr$set_language("en")
  expect_equal(tr_hintr$language(), "en")

  tr_naomi <- traduire::translator(package = "naomi")
  tr_naomi$set_language("en")
  expect_equal(tr_naomi$language(), "en")

  res <- MockPlumberResponse$new()
  req <- list(HEADERS = c("accept-language" = "fr"))
  value <- list("the response")

  expect_null(api_set_language(data, req, res))
  expect_equal(tr_hintr$language(), "fr")
  expect_equal(tr_naomi$language(), "fr")

  expect_identical(api_reset_language(data, req, res, value), value)
  expect_equal(tr_hintr$language(), "en")
  expect_equal(tr_naomi$language(), "en")
})
