context("indicators")

test_that("can get filtered data for indicator", {
  survey <- file_object(file.path("testdata", "survey.csv"))
  indicator_data <- get_indicator_data(survey, "survey", "art_coverage")
  expect_true(all(indicator_data$indicator == "artcov"))
  expect_equal(names(indicator_data),
               c("indicator", "survey_id", "survey_year", "area_id", "sex",
                 "age_group", "n_cluster", "n_obs", "est", "se", "ci_l", "ci_u"))
  prevalence_data <- get_indicator_data(survey, "survey", "prevalence")
  expect_true(all(prevalence_data$indicator == "prev"))

  anc <- file_object(file.path("testdata", "anc.csv"))
  anc_prevalence <- get_indicator_data(anc, "anc", "prevalence")
  expect_equal(names(anc_prevalence),
               c("area_id", "age_group", "year", "anc_clients",
                 "ancrt_hiv_status", "ancrt_known_pos", "ancrt_already_art",
                 "ancrt_tested", "ancrt_test_pos"))
  anc_art <- get_indicator_data(anc, "anc", "art_coverage")
  expect_equal(nrow(anc_art), nrow(anc_prevalence))

  mock_metadata <- mockery::mock(data.frame(
    data_type = c("survey", "survey"),
    indicator = c("prevalence", "art_coverage"),
    stringsAsFactors = FALSE
  ))
  with_mock("naomi::get_metadata" = mock_metadata, {
    expect_error(get_indicator_data(anc, "anc", "prevalence"),
                 "Found 0 rows in metadata for data type anc and indicator prevalence. Should be exactly one.")
  })
})
