context("model-options")

test_that("can build JSON from template", {
  json <- build_json("test <+param1+>", list(param1 = scalar("test string")))
  expect_equal(json, 'test "test string"')

  json <- build_json("<+param1+> test, <+param_2+>",
                     list(param1 = scalar("x"), param_2 = scalar("y")))
  expect_equal(json, '"x" test, "y"')

  json <- build_json('{"options": <+options+>, "test": <+test+>}',
                     list(options = c(scalar("MWI"),
                                      scalar("MWI_1_1"),
                                      scalar("MWI_1_2")),
                          test = scalar("test_value")))
  expect_equal(json,
               '{"options": ["MWI","MWI_1_1","MWI_1_2"], "test": "test_value"}')

  json <- build_json('{"options": <+options+>, "test": <+test+>, "text": "Age < 5"}',
                     list(options = list(
                       list(id = scalar("MWI"),
                            label = scalar("Malawi")),
                       list(id = scalar("MWI_1_1"),
                            label = scalar("Northern")),
                       list(id = scalar("MWI_1_2"),
                            label = scalar("Central"))),
                       test = scalar("test_value")))
  expect_equal(json,
               '{"options": [{"id":"MWI","label":"Malawi"},{"id":"MWI_1_1","label":"Northern"},{"id":"MWI_1_2","label":"Central"}], "test": "test_value", "text": "Age < 5"}')

  ## Additional params are ignored
  json <- build_json("test <+param+>", list(param = scalar("test"), param2 = scalar("test2")))
  expect_equal(json, 'test "test"')

  ## Null params
  json <- build_json('{"options": <+param+>}', list(param = scalar(NULL)))
  expect_equal(json, '{"options": {}}')
})

test_that("JSON build fails if params are missing", {
  expect_error(build_json('{"options": <+param+>}', list(value = "test")),
               "Failed to construct model options from template and params:
object 'param' not found")
})

test_that("do_endpoint_model_options correctly builds params list", {
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  survey <- file_object(file.path("testdata", "survey.csv"))
  art <- file_object(file.path("testdata", "programme.csv"))
  anc <- file_object(file.path("testdata", "anc.csv"))

  mock_build_json <- mockery::mock('"{"test"}')
  with_mock("hintr:::build_json" = mock_build_json,  {
    json <- do_endpoint_model_options(shape, survey, art, anc)
    args <- mockery::mock_args(mock_build_json)
  })
  expect_length(args[[1]], 2)
  expect_true(grepl('"label": "ART"', args[[1]][[1]]))
  expect_true(grepl('"label": "ANC"', args[[1]][[1]]))
  params <- args[[1]][[2]]
  expect_equal(names(params),
               c("area_scope_options", "area_scope_default",
                 "area_level_options", "area_level_default",
                 "calendar_quarter_t1_options", "calendar_quarter_t2_options",
                 "survey_prevalence_options", "survey_art_coverage_options",
                 "survey_recently_infected_options",
                 "anc_prevalence_year1_options",
                 "anc_prevalence_year2_options",
                 "anc_art_coverage_year1_options",
                 "anc_art_coverage_year2_options"))

  expect_length(params$area_scope_options, 1)
  expect_equal(names(params$area_scope_options[[1]]),
               c("id", "label", "children"))
  expect_equal(params$area_scope_options[[1]]$id, scalar("MWI"))
  expect_equal(params$area_scope_options[[1]]$label, scalar("Malawi"))
  expect_length(params$area_scope_options[[1]]$children, 3)
  expect_equal(params$area_scope_default, scalar("MWI"))
  expect_equal(params$area_level_options, list(
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
  t1 <- params$calendar_quarter_t1_options
  expect_equal(t1[[length(t1)]]$id, scalar("CY2010Q1"))
  expect_equal(t1[[length(t1)]]$label, scalar("March 2010"))
  expect_true(length(t1) >= 32)
  t2 <- params$calendar_quarter_t2_options
  expect_equal(t2[[length(t2)]]$id, scalar("CY2010Q1"))
  expect_equal(t2[[length(t2)]]$label, scalar("March 2010"))
  expect_true(length(t2) >= 32)
  expect_length(params$survey_prevalence_options, 4)
  expect_equal(params$survey_prevalence_options[[1]]$id,
               scalar("MWI2016PHIA"))
  expect_equal(params$survey_prevalence_options[[1]]$label,
               scalar("MWI2016PHIA"))
  expect_length(params$survey_art_coverage_options, 1)
  expect_length(params$survey_recently_infected_options, 1)
  expect_length(params$anc_prevalence_year1_options, 8)
  expect_equal(params$anc_prevalence_year1_options[[1]]$id, scalar("2018"))
  expect_equal(params$anc_prevalence_year1_options[[1]]$label, scalar("2018"))
  expect_equal(params$anc_prevalence_year1_options,
               params$anc_prevalence_year2_options)
  expect_equal(params$anc_prevalence_year1_options,
               params$anc_art_coverage_year1_options)
  expect_equal(params$anc_prevalence_year1_options,
               params$anc_art_coverage_year2_options)
})

test_that("do_endpoint_model_options without programme data", {
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  survey <- file_object(file.path("testdata", "survey.csv"))

  mock_build_json <- mockery::mock('"{"test"}')
  with_mock("hintr:::build_json" = mock_build_json,  {
    json <- do_endpoint_model_options(shape, survey, NULL, NULL)
    args <- mockery::mock_args(mock_build_json)
  })
  expect_length(args[[1]], 2)
  expect_false(grepl('"label": "ART"', args[[1]][[1]]))
  expect_false(grepl('"label": "ANC"', args[[1]][[1]]))
  params <- args[[1]][[2]]
  expect_equal(names(params),
               c("area_scope_options", "area_scope_default",
                 "area_level_options", "area_level_default",
                 "calendar_quarter_t1_options", "calendar_quarter_t2_options",
                 "survey_prevalence_options", "survey_art_coverage_options",
                 "survey_recently_infected_options",
                 "anc_prevalence_year1_options",
                 "anc_prevalence_year2_options",
                 "anc_art_coverage_year1_options",
                 "anc_art_coverage_year2_options"))

  expect_length(params$area_scope_options, 1)
  expect_equal(names(params$area_scope_options[[1]]),
               c("id", "label", "children"))
  expect_equal(params$area_scope_options[[1]]$id, scalar("MWI"))
  expect_equal(params$area_scope_options[[1]]$label, scalar("Malawi"))
  expect_length(params$area_scope_options[[1]]$children, 3)
  expect_equal(params$area_scope_default, scalar("MWI"))
  expect_equal(params$area_level_options, list(
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
  t1 <- params$calendar_quarter_t1_options
  expect_equal(t1[[length(t1)]]$id, scalar("CY2010Q1"))
  expect_equal(t1[[length(t1)]]$label, scalar("March 2010"))
  expect_true(length(t1) >= 32)
  t2 <- params$calendar_quarter_t2_options
  expect_equal(t2[[length(t2)]]$id, scalar("CY2010Q1"))
  expect_equal(t2[[length(t2)]]$label, scalar("March 2010"))
  expect_true(length(t2) >= 32)
  expect_length(params$survey_prevalence_options, 4)
  expect_equal(params$survey_prevalence_options[[1]]$id,
               scalar("MWI2016PHIA"))
  expect_equal(params$survey_prevalence_options[[1]]$label,
               scalar("MWI2016PHIA"))
  expect_length(params$survey_art_coverage_options, 1)
  expect_length(params$survey_recently_infected_options, 1)
})

test_that("can retrieve validated model options", {
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  survey <- file_object(file.path("testdata", "survey.csv"))
  art <- file_object(file.path("testdata", "programme.csv"))
  anc <- file_object(file.path("testdata", "anc.csv"))
  json <- do_endpoint_model_options(shape, survey, art, anc)

  json <- jsonlite::parse_json(json)
  expect_equal(names(json), "controlSections")
  expect_length(json$controlSections, 7)

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
  expect_equal(
    general_section$controlGroups[[1]]$controls[[1]]$options[[1]]$label,
    "Malawi"
  )
  expect_equal(
    general_section$controlGroups[[1]]$controls[[1]]$value,
    "MWI")
  expect_length(
    general_section$controlGroups[[2]]$controls[[1]]$options,
    5
  )
  expect_equal(
    names(general_section$controlGroups[[2]]$controls[[1]]$options[[1]]),
    c("id", "label")
  )
  expect_equal(
    general_section$controlGroups[[2]]$controls[[1]]$options[[1]]$id,
    "0")
  expect_equal(
    general_section$controlGroups[[2]]$controls[[1]]$options[[1]]$label,
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
    "MWI2016PHIA")
  expect_equal(
    survey_section$controlGroups[[2]]$controls[[1]]$options[[1]]$label,
    "MWI2016PHIA")

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

  advanced_section <- json$controlSections[[7]]
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

test_that("model options can be validated", {
  skip("Skipping model option validation endpoint not implemented mrc-592")

  data <- list(
    pjnz = "path/to/pjnz",
    shape = "path",
    population = "path",
    survey = "path",
    programme = "path",
    anc = "path"
  )
  options <- list(
    option1 = "one",
    option2 = "two"
  )
  valid <- do_validate_model_options(data, options)
  expect_equal(names(valid), "valid")
  expect_equal(valid$valid, scalar(TRUE))
})

test_that("area level is prepopualted to lowest region", {
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  survey <- file_object(file.path("testdata", "survey.csv"))
  json <- do_endpoint_model_options(shape, survey, NULL, NULL)

  json <- jsonlite::parse_json(json)

  expect_equal(json$controlSections[[1]]$controlGroups[[2]]$label, "Area level")
  expect_equal(json$controlSections[[1]]$controlGroups[[2]]$controls[[1]]$value, "4")
})
