context("run-model")

test_that("model can be run", {
  result <- run_model(list(), list())
  expect_s3_class(result, "data.frame")
  expect_equal(names(result),
               c("area_id", "sex", "agegr_id", "indicator",
                 "time", "mean", "se", "median", "lower", "upper"))
})
