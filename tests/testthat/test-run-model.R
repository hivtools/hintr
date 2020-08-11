context("run-model")

test_that("model can be run and filters extracted", {
  test_mock_model_available()
  model_run <- process_result(mock_model)
  expect_equal(names(model_run), c("data", "plottingMetadata"))
  expect_equal(names(model_run$data),
               c("area_id", "sex", "age_group", "calendar_quarter",
                 "indicator_id", "mode", "mean", "lower", "upper"))
  expect_true(nrow(model_run$data) > 84042)
  expect_equal(names(model_run$plottingMetadata), c("barchart", "choropleth"))
  barchart <- model_run$plottingMetadata$barchart
  expect_equal(names(barchart), c("indicators", "filters", "defaults"))
  expect_length(barchart$filters, 4)
  expect_equal(names(barchart$filters[[1]]),
               c("id", "column_id", "label", "options", "use_shape_regions"))
  expect_equal(names(barchart$filters[[2]]),
               c("id", "column_id", "label", "options"))
  expect_equal(barchart$filters[[1]]$id, scalar("area"))
  expect_equal(barchart$filters[[2]]$id, scalar("quarter"))
  expect_equal(barchart$filters[[3]]$id, scalar("sex"))
  expect_equal(barchart$filters[[4]]$id, scalar("age"))
  expect_true(length(barchart$filters[[4]]$options) >= 29)
  expect_length(barchart$filters[[2]]$options, 3)
  expect_equal(barchart$filters[[2]]$options[[2]]$id, scalar("CY2018Q3"))
  expect_equal(barchart$filters[[2]]$options[[2]]$label, scalar("September 2018"))
  expect_equal(nrow(barchart$indicators), 10)
  expect_true(all(c("prevalence", "art_coverage", "current_art", "population",
                    "plhiv", "incidence", "new_infections", "receiving_art",
                    "anc_prevalence", "anc_art_coverage") %in%
                    barchart$indicators$indicator))

  choropleth <- model_run$plottingMetadata$choropleth
  expect_equal(names(choropleth), c("indicators", "filters"))
  expect_length(choropleth$filters, 4)
  expect_equal(names(choropleth$filters[[1]]),
               c("id", "column_id", "label", "options", "use_shape_regions"))
  expect_equal(names(choropleth$filters[[2]]),
               c("id", "column_id", "label", "options"))
  expect_equal(choropleth$filters[[1]]$id, scalar("area"))
  expect_equal(choropleth$filters[[2]]$id, scalar("quarter"))
  expect_equal(choropleth$filters[[3]]$id, scalar("sex"))
  expect_equal(choropleth$filters[[4]]$id, scalar("age"))
  expect_true(length(choropleth$filters[[4]]$options) >= 29)
  expect_length(choropleth$filters[[2]]$options, 3)
  expect_equal(choropleth$filters[[2]]$options[[2]]$id, scalar("CY2018Q3"))
  expect_equal(choropleth$filters[[2]]$options[[2]]$label,
               scalar("September 2018"))
  expect_equal(nrow(choropleth$indicators), 10)
  expect_true(all(!is.null(choropleth$indicators$error_low_column)))
  expect_true(all(!is.null(choropleth$indicators$error_high_column)))
  expect_true(all(c("prevalence", "art_coverage", "current_art", "population",
                    "plhiv", "incidence", "new_infections", "receiving_art",
                    "anc_prevalence", "anc_art_coverage") %in%
                    choropleth$indicators$indicator))
})

test_that("model without national level results can be processed", {
  test_mock_model_available()
  output <- readRDS(mock_model$output_path)
  output <- output[output$area_level != 0, ]
  output_temp <- tempfile()
  saveRDS(output, output_temp)
  model_run <- process_result(list(output_path = output_temp))
  expect_equal(names(model_run), c("data", "plottingMetadata"))
  expect_equal(names(model_run$data),
               c("area_id", "sex", "age_group", "calendar_quarter",
                 "indicator_id", "mode", "mean", "lower", "upper"))
  expect_true(nrow(model_run$data) > 84042)
  expect_equivalent(as.data.frame(model_run$data)[1, "area_id"], "MWI_1_1")
  expect_equal(names(model_run$plottingMetadata), c("barchart", "choropleth"))
  barchart <- model_run$plottingMetadata$barchart
  expect_equal(names(barchart), c("indicators", "filters", "defaults"))
  expect_length(barchart$filters, 4)
  expect_equal(names(barchart$filters[[1]]),
               c("id", "column_id", "label", "options", "use_shape_regions"))
  expect_equal(names(barchart$filters[[2]]),
               c("id", "column_id", "label", "options"))
  expect_equal(barchart$filters[[1]]$id, scalar("area"))
  expect_equal(barchart$filters[[2]]$id, scalar("quarter"))
  expect_equal(barchart$filters[[3]]$id, scalar("sex"))
  expect_equal(barchart$filters[[4]]$id, scalar("age"))
  expect_true(length(barchart$filters[[4]]$options) >= 29)
  expect_length(barchart$filters[[2]]$options, 3)
  expect_equal(barchart$filters[[2]]$options[[2]]$id, scalar("CY2018Q3"))
  expect_equal(barchart$filters[[2]]$options[[2]]$label, scalar("September 2018"))
  expect_equal(nrow(barchart$indicators), 10)
  expect_true(all(c("prevalence", "art_coverage", "current_art", "population",
                    "plhiv", "incidence", "new_infections", "receiving_art",
                    "anc_prevalence", "anc_art_coverage") %in%
                    barchart$indicators$indicator))

  choropleth <- model_run$plottingMetadata$choropleth
  expect_equal(names(choropleth), c("indicators", "filters"))
  expect_length(choropleth$filters, 4)
  expect_equal(names(choropleth$filters[[1]]),
               c("id", "column_id", "label", "options", "use_shape_regions"))
  expect_equal(names(choropleth$filters[[2]]),
               c("id", "column_id", "label", "options"))
  expect_equal(choropleth$filters[[1]]$id, scalar("area"))
  expect_equal(choropleth$filters[[2]]$id, scalar("quarter"))
  expect_equal(choropleth$filters[[3]]$id, scalar("sex"))
  expect_equal(choropleth$filters[[4]]$id, scalar("age"))
  expect_true(length(choropleth$filters[[4]]$options) >= 29)
  expect_length(choropleth$filters[[2]]$options, 3)
  expect_equal(choropleth$filters[[2]]$options[[2]]$id, scalar("CY2018Q3"))
  expect_equal(choropleth$filters[[2]]$options[[2]]$label,
               scalar("September 2018"))
  expect_equal(nrow(choropleth$indicators), 10)
  expect_true(all(c("prevalence", "art_coverage", "current_art", "population",
                    "plhiv", "incidence", "new_infections", "receiving_art",
                    "anc_prevalence", "anc_art_coverage") %in%
                    choropleth$indicators$indicator))

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
    calendar_quarter_t3 = "CY2019Q2",
    survey_prevalence = c("MWI2016PHIA", "MWI2015DHS"),
    survey_art_coverage = "MWI2016PHIA",
    survey_recently_infected = "MWI2016PHIA",
    include_art_t1 = "true",
    include_art_t2 = "true",
    anc_prevalence_year1 = 2016,
    anc_prevalence_year2 = 2018,
    anc_art_coverage_year1 = 2016,
    anc_art_coverage_year2 = 2018,
    spectrum_population_calibration = "none",
    spectrum_plhiv_calibration_level = "none",
    spectrum_plhiv_calibration_strat = "sex_age_group",
    spectrum_artnum_calibration_level = "none",
    spectrum_artnum_calibration_strat = "age_coarse",
    spectrum_infections_calibration_level = "none",
    spectrum_infections_calibration_strat = "age_coarse",
    artattend_log_gamma_offset = -4L,
    artattend = "false",
    rng_seed = 17,
    no_of_samples = 20,
    max_iter = 250,
    permissive = "false"
  )
  withr::with_envvar(c("USE_MOCK_MODEL" = "false"), {
    model_run <- run_model(data, options, tempdir())
  })
  expect_equal(names(model_run),
               c("output_path", "spectrum_path", "summary_path", "metadata"))

  output <- readRDS(model_run$output_path)
  expect_equal(colnames(output),
               c("area_level", "area_level_label", "area_id", "area_name",
                 "sex", "age_group", "age_group_id", "age_group_label",
                 "calendar_quarter", "quarter_id", "quarter_label", "indicator",
                 "indicator_id", "indicator_label", "mean",
                 "se", "median", "mode", "lower", "upper"))
  expect_true(nrow(output) > 84042)

  file_list <- unzip(model_run$spectrum_path, list = TRUE)
  expect_true(all(c("boundaries.geojson", "indicators.csv", "meta_age_group.csv",
                    "meta_area.csv", "meta_indicator.csv", "meta_period.csv")
                  %in% file_list$Name))

  ## TODO: replace with checks for spectrum digest once function to create
  ## that has been added mrc-636
  file_list <- unzip(model_run$summary_path, list = TRUE)
  expect_true(all(c("boundaries.geojson", "indicators.csv", "meta_age_group.csv",
                    "meta_area.csv", "meta_indicator.csv", "meta_period.csv")
                  %in% file_list$Name))
})

test_that("real model can be run with csv2 data", {
  testthat::skip_on_covr() # slow so don't run over again
  convert_csv <- function(path) {
    dest <- tempfile(fileext = ".csv")
    write.csv2(read_csv(path), dest, row.names = FALSE)
    dest
  }
  data <- list(
    pjnz = file.path("testdata", "Malawi2019.PJNZ"),
    shape = file.path("testdata", "malawi.geojson"),
    population = convert_csv(file.path("testdata", "population.csv")),
    survey = convert_csv(file.path("testdata", "survey.csv")),
    programme = convert_csv(file.path("testdata", "programme.csv")),
    anc = convert_csv(file.path("testdata", "anc.csv"))
  )
  options <- list(
    area_scope = "MWI",
    area_level = 4,
    calendar_quarter_t1 = "CY2016Q1",
    calendar_quarter_t2 = "CY2018Q3",
    calendar_quarter_t3 = "CY2019Q2",
    survey_prevalence = c("MWI2016PHIA", "MWI2015DHS"),
    survey_art_coverage = "MWI2016PHIA",
    survey_recently_infected = "MWI2016PHIA",
    include_art_t1 = "true",
    include_art_t2 = "true",
    anc_prevalence_year1 = 2016,
    anc_prevalence_year2 = 2018,
    anc_art_coverage_year1 = 2016,
    anc_art_coverage_year2 = 2018,
    spectrum_population_calibration = "none",
    spectrum_plhiv_calibration_level = "none",
    spectrum_plhiv_calibration_strat = "sex_age_group",
    spectrum_artnum_calibration_level = "none",
    spectrum_artnum_calibration_strat = "age_coarse",
    spectrum_infections_calibration_level = "none",
    spectrum_infections_calibration_strat = "age_coarse",
    artattend_log_gamma_offset = -4L,
    artattend = "false",
    rng_seed = 17,
    no_of_samples = 20,
    max_iter = 250,
    permissive = "false"
  )
  withr::with_envvar(c("USE_MOCK_MODEL" = "false"), {
    model_run <- run_model(data, options, tempdir())
  })

  expect_equal(names(model_run),
               c("output_path", "spectrum_path", "summary_path", "metadata"))

  output <- readRDS(model_run$output_path)
  expect_equal(colnames(output),
               c("area_level", "area_level_label", "area_id", "area_name",
                 "sex", "age_group", "age_group_id", "age_group_label",
                 "calendar_quarter", "quarter_id", "quarter_label", "indicator",
                 "indicator_id", "indicator_label", "mean",
                 "se", "median", "mode", "lower", "upper"))
  expect_true(nrow(output) > 84042)

  file_list <- unzip(model_run$spectrum_path, list = TRUE)
  expect_true(all(c("boundaries.geojson", "indicators.csv", "meta_age_group.csv",
                    "meta_area.csv", "meta_indicator.csv", "meta_period.csv")
                  %in% file_list$Name))

  ## TODO: replace with checks for spectrum digest once function to create
  ## that has been added mrc-636
  file_list <- unzip(model_run$summary_path, list = TRUE)
  expect_true(all(c("boundaries.geojson", "indicators.csv", "meta_age_group.csv",
                    "meta_area.csv", "meta_indicator.csv", "meta_period.csv")
                  %in% file_list$Name))
})
