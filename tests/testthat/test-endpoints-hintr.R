context("endpoints-hintr")

test_that("endpoint hintr works", {
  response <- endpoint_hintr_version()
  response <- jsonlite::parse_json(response)

  expect_is(response$data, "list")
  expect_setequal(names(response$data), c("hintr", "naomi", "rrq"))
  expect_equal(response$data$rrq, as.character(packageVersion("rrq")))
})
