context("prerun models")

test_that("import failures", {
  obj <- PrerunModelResults$new(tempfile())
  p <- tempfile()
  expect_error(obj$import(p), "Import directory .+ does not exist")
  dir.create(p)
  expect_error(obj$import(p),
               "Path 'model-output.rds' for 'model_output' does not exist")
  file.create(file.path(p, "model-output.rds"))
  expect_error(obj$import(p, "path/to/output.zip"),
               "Path for 'model_output' must be just the filename, no slashes")
})

test_that("import base data", {
  test_mock_model_available()
  path_prerun <- tempfile()
  obj <- PrerunModelResults$new(path_prerun)
  expect_equal(obj$list(), character(0))

  p <- system_file("output")

  inputs <- read_info_inputs(file.path(p, "malawi_model_output.rds"))
  expect_false(obj$exists(inputs))

  h <- obj$import(p, "malawi_model_output.rds")
  expect_equal(obj$list(), h)
  expect_true(obj$exists(inputs))
  expect_equal(obj$get(inputs), list(
    model_output_path = file.path(path_prerun, h, "model-output.rds")))
  expect_true(all(vapply(obj$get(inputs), file.exists, TRUE)))

  expect_error(
    obj$import(p, "malawi_model_output.rds"),
    "This set of data has been imported already")
})

test_that("run with prerun", {
  test_mock_model_available()
  path_prerun <- tempfile()
  obj <- PrerunModelResults$new(path_prerun)
  expect_equal(obj$list(), character(0))

  h <- prerun_import(path_prerun, system_file("output"),
                     "malawi_model_output.rds")

  data <- list(
    pjnz = list(filename = "Malawi2019.PJNZ",
                path = file.path("testdata", "Malawi2019.PJNZ"),
                hash = "a123"),
    shape = list(filename = "malawi.geojson",
                 path = file.path("testdata", "malawi.geojson"),
                 hash = "a123"),
    population = list(filename = "population.csv",
                      path = file.path("testdata", "population.csv"),
                      hash = "a123"),
    survey = list(filename = "survey.csv",
                  path = file.path("testdata", "survey.csv"),
                  hash = "a123"),
    programme = list(filename = "programme.csv",
                     path = file.path("testdata", "programme.csv"),
                     hash = "a123"),
    anc = list(filename = "anc.csv",
               path = file.path("testdata", "anc.csv"),
               hash = "a123")
  )
  options <- list(
    area_scope = "MWI",
    area_level = 4,
    calendar_quarter_t1 = "CY2016Q1",
    calendar_quarter_t2 = "CY2018Q3",
    calendar_quarter_t3 = "CY2019Q2",
    survey_prevalence = c("DEMO2016PHIA", "DEMO2015DHS"),
    survey_art_coverage = "DEMO2016PHIA",
    survey_recently_infected = "DEMO2016PHIA",
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
    calibrate_method = "logistic",
    artattend_log_gamma_offset = -4L,
    artattend = "false",
    rng_seed = 17,
    no_of_samples = 20,
    max_iter = 250
  )

  path_results <- tempfile()
  dir.create(path_results)
  withr::with_envvar(c("USE_MOCK_MODEL" = "false"), {
    expect_message(
      model_run <- run_model(data, options, path_results, path_prerun),
      "Found prerun model results")
  })
  expect_equal(dir(path_results), character(0))
  expect_equal(model_run, obj$get_by_hash(h))
})

test_that("run with prerun", {
  test_mock_model_available()
  path_prerun <- tempfile()
  obj <- PrerunModelResults$new(path_prerun)
  expect_equal(obj$list(), character(0))

  args <- c(path_prerun, system_file("output"),
            "--model-output=malawi_model_output.rds")
  expect_message(main_import_prerun(args),
                 "Imported data as '[[:xdigit:]]+'")
  expect_equal(length(obj$list()), 1)
})
