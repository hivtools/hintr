context("run-model")

test_that("model can be run", {
  expect_equal(run_model(list(), list()), 2)
})
