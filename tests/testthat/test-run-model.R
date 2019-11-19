context("run-model")

test_that("model can be run and filters extracted", {
  test_mock_model_available()
  model_run <- process_result(mock_model)
  expect_equal(names(model_run), c("data", "plottingMetadata"))
  expect_equal(names(model_run$data),
               c("area_id", "sex", "age_group_id", "quarter_id", "indicator_id",
                 "mode", "mean", "lower", "upper"))
  expect_equal(nrow(model_run$data), 42021)
  expect_equal(names(model_run$plottingMetadata), "barchart")
  expect_equal(names(model_run$plottingMetadata$barchart),
               c("indicators", "filters"))
  expect_length(model_run$plottingMetadata$barchart$filters, 2)
  expect_equal(names(model_run$plottingMetadata$barchart$filters[[1]]),
               c("id", "column_id", "label", "options"))
  expect_equal(model_run$plottingMetadata$barchart$filters[[1]]$id,
               scalar("age"))
  expect_equal(model_run$plottingMetadata$barchart$filters[[2]]$id,
               scalar("quarter"))
  expect_length(model_run$plottingMetadata$barchart$filters[[1]]$options, 29)
  expect_length(model_run$plottingMetadata$barchart$filters[[2]]$options, 1)
  expect_equal(model_run$plottingMetadata$barchart$filters[[2]]$options[[1]]$label,
               scalar("Jan-Mar 2016"))
  expect_length(model_run$plottingMetadata$barchart$indicators, 7)
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
    calendar_quarter_t1 = "CY2016Q1",
    calendar_quarter_t2 = "CY2018Q3",
    survey_prevalence = c("MWI2016PHIA", "MWI2015DHS"),
    survey_art_coverage = "MWI2016PHIA",
    survey_recently_infected = "MWI2016PHIA",
    survey_art_or_vls = "art_coverage",
    include_art = "true",
    anc_prevalence_year1 = 2016,
    anc_prevalence_year2 = 2018,
    anc_art_coverage_year1 = 2016,
    anc_art_coverage_year2 = 2018,
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
                 "sex", "age_group", "age_group_id", "age_group_label",
                 "calendar_quarter", "quarter_id", "quarter_label", "indicator"
                 "indicator_id", "indicator_label", "mode",
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
