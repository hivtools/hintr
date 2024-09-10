test_that("can get data for calibration plot", {
  test_redis_available()
  test_mock_model_available()

  q <- test_queue_result()

  endpoint <- comparison_plot(q$queue)
  res <- endpoint(q$calibrate_id)
  expect_setequal(names(res), c("data", "metadata"))
  expect_setequal(names(res$data),
                  c("area_id", "area_name", "area_level", "age_group", "sex",
                    "calendar_quarter", "indicator", "source", "mean",
                    "lower", "upper"))
  expect_true(nrow(res$data) > 0)
  expect_false(any(is.na(res$data$mean)))
  expect_comparison_metadata(res$metadata)
})
