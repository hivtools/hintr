context("run-model")

test_that("model can be run and filters extracted", {
  test_mock_model_available()
  model_run <- process_result(mock_model)
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

test_that("real model can be run", {
  data <- list(
    pjnz = file.path("testdata", "Malawi2019.PJNZ"),
    shape = file.path("testdata", "malawi.geojson"),
    population = file.path("testdata", "population.csv"),
    survey = file.path("testdata", "survey.csv"),
    programme = file.path("testdata", "programme.csv"),
    anc = file.path("testdata", "anc.csv")
  )
  options <- list(
    area_scope = "MWI",
    area_level = 4,
    t1 = 465,
    t2 = 475,
    survey_prevalence = c("MWI2016PHIA", "MWI2015DHS"),
    survey_art_coverage = "MWI2016PHIA",
    survey_recently_infected = "MWI2016PHIA",
    survey_art_or_vls = "art_coverage",
    art_t1 = 465,
    art_t2 = 475,
    anc_prevalence_t1 = 464,
    anc_prevalence_t2 = 475,
    anc_art_coverage_t1 = 464,
    anc_art_coverage_t2 = 475,
    no_of_samples = 20
  )
  withr::with_envvar(c("USE_MOCK_MODEL" = "false"), {
    model_run <- run_model(data, options)
  })
  expect_equal(names(model_run),
               c("output_path", "spectrum_path", "summary_path"))

  output <- readRDS(model_run$output_path)
  expect_equal(colnames(output),
               c("area_level", "area_level_label", "area_id", "area_name",
                 "sex", "age_group_id", "age_group_label", "quarter_id",
                 "quarter_label", "indicator_id", "indicator_label", "mode",
                 "mean", "se", "median", "lower", "upper"))
  expect_true(nrow(output) == 42021)

  file_list <- unzip(model_run$spectrum_path, list = TRUE)
  expect_equal(file_list$Name,
               c("boundaries.geojson", "indicators.csv", "meta_age_group.csv",
                 "meta_area.csv", "meta_indicator.csv", "meta_period.csv"))

  ## TODO: replace with checks for spectrum digest once function to create
  ## that has been added mrc-636
  file_list <- unzip(model_run$summary_path, list = TRUE)
  expect_equal(file_list$Name,
               c("boundaries.geojson", "indicators.csv", "meta_age_group.csv",
                 "meta_area.csv", "meta_indicator.csv", "meta_period.csv"))
})
