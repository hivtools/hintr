test_that("can get data for calibration plot", {
  test_redis_available()
  test_mock_model_available()

  q <- test_queue_result()

  endpoint <- comparison_plot(q$queue)
  res <- endpoint(q$calibrate_id)
  expect_setequal(names(res), c("data", "plottingMetadata"))
  expect_setequal(names(res$data),
                  c("area_id", "area_name", "age_group", "sex",
                    "calendar_quarter", "indicator", "source", "mean",
                    "lower", "upper"))
  expect_true(nrow(res$data) > 0)
  expect_equal(names(res$plottingMetadata), "barchart")
  expect_setequal(names(res$plottingMetadata$barchart),
                  c("indicators", "filters", "defaults", "selections"))

  expect_setequal(names(res$plottingMetadata$barchart$indicators),
                  c("indicator", "value_column", "error_low_column",
                    "error_high_column", "indicator_column", "indicator_value",
                    "indicator_sort_order", "name", "scale", "accuracy",
                    "format"))
  expect_true(nrow(res$plottingMetadata$barchart$indicators) > 0)

  filters <- lapply(res$plottingMetadata$barchart$filters, function(filter) {
    filter$column_id
  })
  expect_equal(filters[[1]], scalar("area_id"))
  expect_equal(filters[[2]], scalar("calendar_quarter"))
  expect_equal(filters[[3]], scalar("sex"))
  expect_equal(filters[[4]], scalar("age_group"))
  expect_equal(filters[[5]], scalar("source"))

  expect_setequal(names(res$plottingMetadata$barchart$defaults),
                  c("indicator_id", "x_axis_id", "disaggregate_by_id",
                    "selected_filter_options"))

  expect_true(length(res$plottingMetadata$barchart$selections) >= 5)
  for (selection in res$plottingMetadata$barchart$selections) {
    expect_setequal(names(selection),
                    c("indicator_id", "x_axis_id", "disaggregate_by_id",
                      "selected_filter_options"))
  }

  expect_false(any(is.na(res$data$mean)))
})
