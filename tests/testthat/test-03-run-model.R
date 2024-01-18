test_that("model can be run & calibrated", {
  test_mock_model_available()
  res <- process_result(mock_calibrate)
  expect_equal(names(res),
               c("data", "warnings"))
  expect_equal(names(res$data),
               c("area_id", "sex", "age_group", "calendar_quarter",
                 "indicator", "mode", "mean", "lower", "upper"))
  expect_true(nrow(res$data) > 84042)
})

test_that("model without national level results can be processed", {
  test_mock_model_available()
  output <- naomi::read_hintr_output(mock_calibrate$plot_data_path)
  output <- output[output$area_level != 0, ]
  output_temp <- tempfile(fileext = ".rds")
  saveRDS(output, output_temp)
  res <- process_result(list(plot_data_path = output_temp))
  expect_equal(names(res),
               c("data", "warnings"))
  expect_equal(names(res$data),
               c("area_id", "sex", "age_group", "calendar_quarter",
                 "indicator", "mode", "mean", "lower", "upper"))
  expect_true(nrow(res$data) > 84042)
  expect_equal(as.data.frame(res$data)[1, "area_id"], "MWI_1_1_demo",
               ignore_attr = TRUE)
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
    calendar_quarter_t4 = "CY2022Q3",
    calendar_quarter_t5 = "CY2023Q3",
    survey_prevalence = c("DEMO2016PHIA", "DEMO2015DHS"),
    survey_art_coverage = "DEMO2016PHIA",
    survey_recently_infected = "DEMO2016PHIA",
    include_art_t1 = "true",
    include_art_t2 = "true",
    anc_clients_year2 = 2018,
    anc_clients_year2_num_months = "9",
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
    calibrate_method = "logistic",
    artattend_log_gamma_offset = -4L,
    artattend = "false",
    output_aware_plhiv = "true",
    rng_seed = 17,
    no_of_samples = 20,
    max_iter = 250,
    psnu_level = NULL
  )
  withr::with_envvar(c("USE_MOCK_MODEL" = "false"), {
    model_run <- run_model(data, options, tempdir())
  })
  expect_equal(names(model_run), c("plot_data_path", "model_output_path",
                                   "version", "warnings"))

  output <- naomi::read_hintr_output(model_run$model_output_path)
  expect_setequal(names(output),
                  c("output_package", "naomi_data", "info", "warnings"))
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
    calendar_quarter_t4 = "CY2022Q3",
    calendar_quarter_t5 = "CY2023Q3",
    survey_prevalence = c("DEMO2016PHIA", "DEMO2015DHS"),
    survey_art_coverage = "DEMO2016PHIA",
    survey_recently_infected = "DEMO2016PHIA",
    include_art_t1 = "true",
    include_art_t2 = "true",
    anc_clients_year2 = 2018,
    anc_clients_year2_num_months = "9",
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
    calibrate_method = "logistic",
    artattend_log_gamma_offset = -4L,
    artattend = "false",
    output_aware_plhiv = "true",
    rng_seed = 17,
    no_of_samples = 20,
    max_iter = 250,
    psnu_level = NULL
  )
  withr::with_envvar(c("USE_MOCK_MODEL" = "false"), {
    model_run <- run_model(data, options, tempdir())
  })
  expect_equal(names(model_run), c("plot_data_path", "model_output_path",
                                   "version", "warnings"))

  output <- naomi::read_hintr_output(model_run$model_output_path)
  expect_setequal(names(output),
                  c("output_package", "naomi_data", "info", "warnings"))
})

test_that("mock model can be forced to error", {
  data <- list(
    pjnz = file.path("testdata", "Malawi2019.PJNZ"),
    shape = file.path("testdata", "malawi.geojson"),
    population = file.path("testdata", "population.csv"),
    survey = file.path("testdata", "survey.csv"),
    programme = file.path("testdata", "programme.csv"),
    anc = file.path("testdata", "anc.csv")
  )
  options <- list(
    mock_model_trigger_error = "true",
    area_scope = "MWI",
    area_level = 4,
    calendar_quarter_t1 = "CY2016Q1",
    calendar_quarter_t2 = "CY2018Q3",
    calendar_quarter_t3 = "CY2019Q2",
    calendar_quarter_t4 = "CY2022Q3",
    calendar_quarter_t5 = "CY2023Q3",
    survey_prevalence = c("DEMO2016PHIA", "DEMO2015DHS"),
    survey_art_coverage = "DEMO2016PHIA",
    survey_recently_infected = "DEMO2016PHIA",
    include_art_t1 = "true",
    include_art_t2 = "true",
    anc_clients_year2 = 2018,
    anc_clients_year2_num_months = "9",
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
    calibrate_method = "logistic",
    artattend_log_gamma_offset = -4L,
    artattend = "false",
    output_aware_plhiv = "true",
    rng_seed = 17,
    no_of_samples = 20,
    max_iter = 250,
    psnu_level = NULL
  )
  expect_error(
    run_model(data, options, tempdir()),
    "Mock model has errored because option 'mock_model_trigger_error' is TRUE"
  )
})
