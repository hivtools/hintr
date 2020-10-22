context("indicators")

test_that("can get filtered data for indicator", {
  survey <- file_object(file.path("testdata", "survey.csv"))
  indicator_data <- get_indicator_data(survey, "survey", "art_coverage")
  expect_true(all(indicator_data$indicator == "art_coverage"))
  expect_equal(names(indicator_data),
               c("indicator", "survey_id", "survey_year", "area_id", "sex",
                 "age_group", "n_clusters", "n_observations", "n_eff_kish",
                 "estimate", "std_error", "ci_lower", "ci_upper"))
  prevalence_data <- get_indicator_data(survey, "survey", "prevalence")
  expect_true(all(prevalence_data$indicator == "prevalence"))

  anc <- file_object(file.path("testdata", "anc.csv"))
  anc_prevalence <- get_indicator_data(anc, "anc", "anc_prevalence")
  expect_equal(names(anc_prevalence),
               c("area_id", "age_group", "year", "anc_clients",
                 "anc_hiv_status", "anc_known_pos", "anc_already_art",
                 "anc_tested", "anc_tested_pos"))
  anc_art <- get_indicator_data(anc, "anc", "anc_art_coverage")
  expect_equal(nrow(anc_art), nrow(anc_prevalence))

  mock_metadata <- mockery::mock(data.frame(
    data_type = c("survey", "survey"),
    indicator = c("prevalence", "art_coverage"),
    stringsAsFactors = FALSE
  ))
  with_mock("naomi::get_metadata" = mock_metadata, {
    expect_error(get_indicator_data(anc, "anc", "anc_prevalence"),
                 "Found 0 rows in metadata for data type anc and indicator anc_prevalence. Should be exactly one.")
  })
})
