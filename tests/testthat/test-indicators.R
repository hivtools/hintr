test_that("can get filtered data for indicator", {
  data <- read_csv(file.path("testdata", "survey.csv"))
  metadata <- naomi::get_metadata()
  indicator_data <- get_indicator_data(data, metadata, "survey", "art_coverage")
  expect_true(all(indicator_data$indicator == "art_coverage"))
  expect_equal(names(indicator_data),
               c("indicator", "survey_id", "survey_mid_calendar_quarter",
                 "area_id", "area_name", "res_type", "sex", "age_group",
                 "n_clusters", "n_observations", "n_eff_kish", "estimate",
                 "std_error", "ci_lower", "ci_upper"))
  prevalence_data <- get_indicator_data(data, metadata, "survey", "prevalence")
  expect_true(all(prevalence_data$indicator == "prevalence"))

  mock_metadata <- data.frame(
    data_type = c("survey", "survey"),
    indicator = c("prevalence", "art_coverage"),
    stringsAsFactors = FALSE
  )
  expect_error(get_indicator_data(anc, mock_metadata, "anc", "anc_prevalence"),
               "Found 0 rows in metadata for data type anc and indicator anc_prevalence. Should be exactly one.")
})
