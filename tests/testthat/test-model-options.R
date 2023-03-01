test_that("do_endpoint_model_options correctly builds options and fallbacks", {
  shape <- file_object(test_path("testdata", "malawi.geojson"))
  survey <- file_object(test_path("testdata", "survey.csv"))
  art <- file_object(test_path("testdata", "programme.csv"))
  anc <- file_object(test_path("testdata", "anc.csv"))

  mock_get_controls_json <- mockery::mock('"{"test"}')
  with_mock(get_controls_json = mock_get_controls_json,  {
    json <- do_endpoint_model_options(shape, survey, art, anc)
    args <- mockery::mock_args(mock_get_controls_json)
  })
  options <- args[[1]][[3]]
  expect_setequal(names(options),
                  c("area_scope", "area_level",
                    "calendar_quarter_t1", "calendar_quarter_t2",
                    "survey_prevalence", "survey_art_coverage",
                    "survey_recently_infected", "anc_clients_year2",
                    "anc_prevalence_year1", "anc_prevalence_year2",
                    "anc_art_coverage_year1", "anc_art_coverage_year2",
                    "psnu_level"))

  expect_length(options$area_scope, 1)
  expect_equal(names(options$area_scope[[1]]),
               c("id", "label", "children"))
  expect_equal(options$area_scope[[1]]$id, scalar("MWI"))
  expect_equal(options$area_scope[[1]]$label, scalar("Malawi - Demo"))
  expect_length(options$area_scope[[1]]$children, 3)
  expect_equal(options$area_level, list(
    list(
      id = scalar("0"),
      label = scalar("Country")
    ),
    list(
      id = scalar("1"),
      label = scalar("Region")
    ),
    list(
      id = scalar("2"),
      label = scalar("Zone")
    ),
    list(
      id = scalar("3"),
      label = scalar("District")
    ),
    list(
      id = scalar("4"),
      label = scalar("District + Metro")
    )))
  t1 <- options$calendar_quarter_t1
  expect_equal(t1[[length(t1)]]$id, scalar("CY2010Q1"))
  expect_equal(t1[[length(t1)]]$label, scalar("March 2010"))
  expect_true(length(t1) >= 32)

  t2 <- options$calendar_quarter_t2
  expect_equal(t2[[length(t2)]]$id, scalar("CY2010Q1"))
  expect_equal(t2[[length(t2)]]$label, scalar("March 2010"))
  expect_true(length(t2) >= 32)
  expect_length(options$survey_prevalence, 4)
  expect_equal(options$survey_prevalence[[1]]$id,
               scalar("DEMO2016PHIA"))
  expect_equal(options$survey_prevalence[[1]]$label,
               scalar("DEMO2016PHIA"))
  expect_length(options$survey_art_coverage, 1)
  expect_length(options$survey_recently_infected, 1)
  expect_length(options$anc_prevalence_year1, 8)
  expect_equal(options$anc_prevalence_year1[[1]]$id, scalar("2018"))
  expect_equal(options$anc_prevalence_year1[[1]]$label, scalar("2018"))
  expect_equal(options$anc_prevalence_year1,
               options$anc_prevalence_year2)
  expect_equal(options$anc_prevalence_year1,
               options$anc_art_coverage_year1)
  expect_equal(options$anc_prevalence_year1,
               options$anc_art_coverage_year2)

  overrides <- args[[1]][[4]]
  expect_setequal(names(overrides),
                  c("area_scope",
                    "calendar_quarter_t1", "survey_prevalence",
                    "survey_art_coverage", "anc_prevalence_year1",
                    "anc_prevalence_year2", "anc_art_coverage_year1",
                    "anc_art_coverage_year2"))
  expect_equal(overrides$area_scope, scalar("MWI"))
  expect_equal(overrides$calendar_quarter_t1, scalar("CY2016Q1"))
  expect_equal(overrides$survey_prevalence, scalar("DEMO2016PHIA"))
  expect_equal(overrides$survey_art_coverage, scalar("DEMO2016PHIA"))
  expect_equal(overrides$anc_prevalence_year1, scalar("2016"))
  expect_equal(overrides$anc_prevalence_year2, scalar(""))
  expect_equal(overrides$anc_art_coverage_year1, scalar("2016"))
  expect_equal(overrides$anc_art_coverage_year2, scalar(""))
})

test_that("do_endpoint_model_options without programme data", {
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  survey <- file_object(file.path("testdata", "survey.csv"))

  mock_get_controls_json <- mockery::mock('"{"test"}')
  with_mock(get_controls_json = mock_get_controls_json,  {
    json <- do_endpoint_model_options(shape, survey, NULL, NULL)
    args <- mockery::mock_args(mock_get_controls_json)
  })
  options <- args[[1]][[3]]
  expect_setequal(names(options),
                  c("area_scope", "area_level",
                    "calendar_quarter_t1", "calendar_quarter_t2",
                    "survey_prevalence", "survey_art_coverage",
                    "survey_recently_infected", "anc_clients_year2",
                    "anc_prevalence_year1", "anc_prevalence_year2",
                    "anc_art_coverage_year1", "anc_art_coverage_year2",
                    "psnu_level"))

  expect_length(options$area_scope, 1)
  expect_equal(names(options$area_scope[[1]]),
               c("id", "label", "children"))
  expect_equal(options$area_scope[[1]]$id, scalar("MWI"))
  expect_equal(options$area_scope[[1]]$label, scalar("Malawi - Demo"))
  expect_length(options$area_scope[[1]]$children, 3)
  expect_equal(options$area_level, list(
    list(
      id = scalar("0"),
      label = scalar("Country")
    ),
    list(
      id = scalar("1"),
      label = scalar("Region")
    ),
    list(
      id = scalar("2"),
      label = scalar("Zone")
    ),
    list(
      id = scalar("3"),
      label = scalar("District")
    ),
    list(
      id = scalar("4"),
      label = scalar("District + Metro")
    )))
  t1 <- options$calendar_quarter_t1
  expect_equal(t1[[length(t1)]]$id, scalar("CY2010Q1"))
  expect_equal(t1[[length(t1)]]$label, scalar("March 2010"))
  expect_true(length(t1) >= 32)
  t2 <- options$calendar_quarter_t2
  expect_equal(t2[[length(t2)]]$id, scalar("CY2010Q1"))
  expect_equal(t2[[length(t2)]]$label, scalar("March 2010"))
  expect_true(length(t2) >= 32)
  expect_length(options$survey_prevalence, 4)
  expect_equal(options$survey_prevalence[[1]]$id,
               scalar("DEMO2016PHIA"))
  expect_equal(options$survey_prevalence[[1]]$label,
               scalar("DEMO2016PHIA"))
  expect_length(options$survey_art_coverage, 1)
  expect_length(options$survey_recently_infected, 1)

  fallback <- args[[1]][[4]]
  expect_equal(fallback$area_scope, scalar("MWI"))
})

test_that("do_endpoint_model_options overrides anc year2 to 2022 if in data", {
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  survey <- file_object(file.path("testdata", "survey.csv"))
  art <- file_object(file.path("testdata", "programme.csv"))
  anc <- file_object(file.path("testdata", "anc.csv"))

  mock_get_controls_json <- mockery::mock('"{"test"}')
  mock_get_years <- mockery::mock(c(2022, 2021, 2020, 2019))
  with_mock(get_controls_json = mock_get_controls_json,
            get_years = mock_get_years, {
    json <- do_endpoint_model_options(shape, survey, art, anc)
    args <- mockery::mock_args(mock_get_controls_json)
  })
  overrides <- args[[1]][[4]]
  expect_equal(overrides$anc_prevalence_year2, scalar("2022"))
  expect_equal(overrides$anc_art_coverage_year2, scalar("2022"))
  ## Year 1 defaults are NULL as survey year not in ANC years
  expect_equal(overrides$anc_prevalence_year1, scalar(""))
  expect_equal(overrides$anc_art_coverage_year1, scalar(""))
})

test_that("can retrieve validated model options including additional options", {
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  survey <- file_object(file.path("testdata", "survey.csv"))
  art <- file_object(file.path("testdata", "programme.csv"))
  anc <- file_object(file.path("testdata", "anc.csv"))
  json <- do_endpoint_model_options(shape, survey, art, anc)

  json <- jsonlite::parse_json(json)
  expect_equal(names(json), "controlSections")
  expect_length(json$controlSections, 6)

  general_section <- json$controlSections[[1]]
  ## Additional option
  expect_equal(
    general_section$controlGroups[[1]]$controls[[1]]$name,
    "mock_model_trigger_error"
  )
  expect_length(
    general_section$controlGroups[[1]]$controls[[1]]$options, 2)
  expect_length(
    general_section$controlGroups[[2]]$controls[[1]]$options, 1)
  expect_equal(
    names(general_section$controlGroups[[2]]$controls[[1]]$options[[1]]),
    c("id", "label", "children")
  )
  expect_equal(
    general_section$controlGroups[[2]]$controls[[1]]$options[[1]]$id,
    "MWI"
  )
  expect_equal(
    general_section$controlGroups[[2]]$controls[[1]]$options[[1]]$label,
    "Malawi - Demo"
  )
  expect_equal(
    general_section$controlGroups[[2]]$controls[[1]]$value,
    "MWI")
  expect_length(
    general_section$controlGroups[[3]]$controls[[1]]$options,
    5
  )
  expect_equal(
    names(general_section$controlGroups[[3]]$controls[[1]]$options[[1]]),
    c("id", "label")
  )
  expect_equal(
    general_section$controlGroups[[3]]$controls[[1]]$options[[1]]$id,
    "0")
  expect_equal(
    general_section$controlGroups[[3]]$controls[[1]]$options[[1]]$label,
    "Country")

  survey_section <- json$controlSections[[2]]
  expect_true(
    length(survey_section$controlGroups[[1]]$controls[[1]]$options) >
    32
  )
  survey_section <- json$controlSections[[2]]
  expect_length(
    survey_section$controlGroups[[2]]$controls[[1]]$options,
    4
  )
  expect_equal(
    names(survey_section$controlGroups[[1]]$controls[[1]]$options[[1]]),
    c("id", "label"))
  expect_equal(
    survey_section$controlGroups[[2]]$controls[[1]]$options[[1]]$id,
    "DEMO2016PHIA")
  expect_equal(
    survey_section$controlGroups[[2]]$controls[[1]]$options[[1]]$label,
    "DEMO2016PHIA")

  anc_section <- json$controlSections[[3]]
  expect_length(
    anc_section$controlGroups[[1]]$controls[[1]]$options,
    8
  )
  expect_equal(
    names(anc_section$controlGroups[[1]]$controls[[1]]$options[[1]]),
    c("id", "label"))
  expect_equal(
    anc_section$controlGroups[[1]]$controls[[1]]$options[[1]]$id,
    "2018")
  expect_equal(
    anc_section$controlGroups[[1]]$controls[[1]]$options[[1]]$label,
    "2018")

  art_section <- json$controlSections[[4]]
  expect_length(
    art_section$controlGroups[[1]]$controls[[1]]$options,
    2
  )
  expect_equal(
    names(art_section$controlGroups[[1]]$controls[[1]]$options[[1]]),
    c("id", "label"))
  expect_equal(
    art_section$controlGroups[[1]]$controls[[1]]$options[[1]]$id,
    "true")
  expect_equal(
    art_section$controlGroups[[1]]$controls[[1]]$options[[1]]$label,
    "Yes")
  expect_equal(
    art_section$controlGroups[[1]]$controls[[1]]$options[[2]]$id,
    "false")
  expect_equal(
    art_section$controlGroups[[1]]$controls[[1]]$options[[2]]$label,
    "No")

  advanced_section <- json$controlSections[[6]]
  expect_equal(advanced_section$label, "Advanced")
})


test_that("can read geojson level labels", {
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  json <- hintr_geojson_read(shape)
  levels <- get_level_options(json)
  expect_length(levels, 5)
  expect_equal(levels, list(
    list(
      id = scalar("0"),
      label = scalar("Country")
    ),
    list(
      id = scalar("1"),
      label = scalar("Region")
    ),
    list(
      id = scalar("2"),
      label = scalar("Zone")
    ),
    list(
      id = scalar("3"),
      label = scalar("District")
    ),
    list(
      id = scalar("4"),
      label = scalar("District + Metro")
    )))
})


test_that("integrating time options works", {

  ids1 <- c("CY2015Q4", "CY2018Q3")
  ids2 <- c("CY2017Q3", "CY2018Q3")

  quarter_ids1 <- naomi::calendar_quarter_to_quarter_id(ids1)
  quarter_ids2 <- naomi::calendar_quarter_to_quarter_id(ids2)

  times1 <- quarter_id_to_json_list(quarter_ids1)
  times2 <- quarter_id_to_json_list(quarter_ids2)

  times <- union_time_list(times1, times2)

  expect_equal(time_list_ids(times), c("CY2018Q3", "CY2017Q3", "CY2015Q4"))

  times_asc <- union_time_list(times1, times2, decreasing = FALSE)
  times_asc2 <- union_time_list(times2, times1, decreasing = FALSE)
  expect_equal(time_list_ids(times_asc), c("CY2015Q4", "CY2017Q3", "CY2018Q3"))
  expect_equal(time_list_ids(times_asc2), c("CY2015Q4", "CY2017Q3", "CY2018Q3"))
})

test_that("model options work when survey_mid_calendar_quarter missing", {
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  art <- file_object(file.path("testdata", "programme.csv"))
  anc <- file_object(file.path("testdata", "anc.csv"))

  ## Setup survey data for testing
  survey <- file_object(file.path("testdata", "survey.csv"))
  survey_data <- read_csv(survey$path)
  survey_data$survey_mid_calendar_quarter <- NULL
  t <- tempfile()
  write.csv(survey_data, t)
  survey$path <- t

  mock_get_controls_json <- mockery::mock('"{"test"}')
  with_mock(get_controls_json = mock_get_controls_json,  {
    json <- do_endpoint_model_options(shape, survey, art, anc)
    args <- mockery::mock_args(mock_get_controls_json)
  })

  ## Fallback set to most recent time option
  fallback <- args[[1]][[4]]
  time_options <- get_time_options()
  expect_equal(fallback$calendar_quarter_t1, time_options[[1]]$id)
})

test_that("can get survey options & default for different indicators", {
  data <- read_csv(file.path("testdata", "survey.csv"))
  metadata <- naomi::get_metadata()
  prev_options <- get_survey_options(data, metadata, "prevalence")
  expect_equal(prev_options$default, scalar("DEMO2016PHIA"))
  expect_equal(prev_options$options, list(
    list(
      id = scalar("DEMO2016PHIA"),
      label = scalar("DEMO2016PHIA")
    ),
    list(
      id = scalar("DEMO2015DHS"),
      label = scalar("DEMO2015DHS")
    ),
    list(
      id = scalar("DEMO2010DHS"),
      label = scalar("DEMO2010DHS")
    ),
    list(
      id = scalar("DEMO2004DHS"),
      label = scalar("DEMO2004DHS")
    )
  ))

  art_options <- get_survey_options(data, metadata, "art_coverage")
  expect_equal(art_options$default, scalar("DEMO2016PHIA"))
  expect_equal(art_options$options, list(
    list(id = scalar("DEMO2016PHIA"),
         label = scalar("DEMO2016PHIA"))
  ))

  mock_get_indicator_data <- mockery::mock(NULL)
  recent_infected_options <- get_survey_options(data, metadata,
                                                "recent_infected")
  expect_equal(art_options$default, scalar("DEMO2016PHIA"))
  expect_equal(art_options$options, list(
    list(id = scalar("DEMO2016PHIA"),
         label = scalar("DEMO2016PHIA"))
  ))
})

test_that("getting survey options for missing indicator returns empty values", {
  data <- read_csv(file.path("testdata", "survey_prevalence_only.csv"))
  metadata <- naomi::get_metadata()
  expect_equal(
    get_survey_options(data, metadata, "art_coverage"),
    list(
      options = NULL,
      default = scalar("")
    )
  )
})

test_that("when running full model, option to error is not returned", {
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  survey <- file_object(file.path("testdata", "survey.csv"))
  art <- file_object(file.path("testdata", "programme.csv"))
  anc <- file_object(file.path("testdata", "anc.csv"))
  json <- withr::with_envvar(c("USE_MOCK_MODEL" = "false"), {
    do_endpoint_model_options(shape, survey, art, anc)
  })

  json <- jsonlite::parse_json(json)
  expect_equal(names(json), "controlSections")
  expect_length(json$controlSections, 6)

  general_section <- json$controlSections[[1]]
  expect_length(
    general_section$controlGroups[[1]]$controls[[1]]$options, 1)
  expect_equal(
    names(general_section$controlGroups[[1]]$controls[[1]]$options[[1]]),
    c("id", "label", "children")
  )
  expect_equal(
    general_section$controlGroups[[1]]$controls[[1]]$options[[1]]$id,
    "MWI"
  )
})
