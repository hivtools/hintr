context("run-model")

test_that("model can be run and filters extracted", {
  # model_run <- run_model(NULL, list(sleep = 0.1))
  # expect_equal(names(model_run), c("data", "filters"))
  # expect_equal(names(model_run$data),
  #              c("area_id", "sex", "age_group_id", "quarter_id", "indicator_id",
  #                "mode", "mean", "lower", "upper"))
  # expect_equal(nrow(model_run$data), 42021)
  # expect_equal(names(model_run$filters), c("age", "quarter", "indicators"))
  # expect_length(model_run$filters$age, 29)
  # expect_length(model_run$filters$quarter, 1)
  # expect_equal(model_run$filters$quarter[[1]]$label, scalar("Jan-Mar 2016"))
  # expect_length(model_run$filters$indicators, 7)
  # expect_equal(model_run$filters$indicators[[1]]$id, scalar("population"))
  # expect_equal(model_run$filters$indicators[[1]]$label, scalar("Population"))
})

test_that("model result can be processed for return", {
  model_run <- do_process_result(mock_model)
  expect_equal(names(model_run), c("data", "filters"))
  expect_equal(names(model_run$data),
               c("area_id", "sex", "age_group_id", "quarter_id", "indicator_id",
                 "mode", "mean", "lower", "upper"))
  expect_equal(nrow(model_run$data), 42021)
  expect_equal(names(model_run$filters), c("age", "quarter", "indicators"))
  expect_length(model_run$filters$age, 29)
  expect_length(model_run$filters$quarter, 1)
  expect_equal(model_run$filters$quarter[[1]]$label, scalar("Jan-Mar 2016"))
  expect_length(model_run$filters$indicators, 7)
  expect_equal(model_run$filters$indicators[[1]]$id, scalar("population"))
  expect_equal(model_run$filters$indicators[[1]]$label, scalar("Population"))
})
