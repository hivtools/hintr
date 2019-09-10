context("run-model")

test_that("model can be run", {
  expect_equal(run_model(NULL, list(sleep = 0.1)), 2)
})
