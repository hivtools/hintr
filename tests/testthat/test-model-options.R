context("model-options")

test_that("can build JSON from template", {
  json <- build_json("test <param1>", list(param1 = scalar("test string")))
  expect_equal(json, 'test "test string"')

  json <- build_json("<param1> test, <param_2>",
                     list(param1 = scalar("x"), param_2 = scalar("y")))
  expect_equal(json, '"x" test, "y"')

  json <- build_json('{"options": <options>, "test": <test>}',
                     list(options = c(scalar("MWI"), scalar("MWI.1"), scalar("MWI.2")),
                          test = scalar("test_value")))
  expect_equal(json,
               '{"options": ["MWI","MWI.1","MWI.2"], "test": "test_value"}')

  json <- build_json('{"options": <options>, "test": <test>}',
                     list(options = list(
                       list(id = scalar("MWI"),
                            label = scalar("Malawi")),
                       list(id = scalar("MWI.1"),
                            label = scalar("Northern")),
                       list(id = scalar("MWI.2"),
                            label = scalar("Central"))),
                       test = scalar("test_value")))
  expect_equal(json,
               '{"options": [{"id":"MWI","label":"Malawi"},{"id":"MWI.1","label":"Northern"},{"id":"MWI.2","label":"Central"}], "test": "test_value"}')

  ## Additional params are ignored
  json <- build_json("test <param>", list(param = scalar("test"), param2 = scalar("test2")))
  expect_equal(json, 'test "test"')

  ## Null params
  json <- build_json('{"options": <param>}', list(param = scalar(NULL)))
  expect_equal(json, '{"options": {}}')
})

test_that("JSON build fails if params are missing", {
  expect_error(build_json('{"options": <param>}', list(value = "test")),
               "Failed to construct model options from template and params:\n\\w+")
})

test_that("do_endpoint_model_options correctly builds params list", {
  shape <- file.path("testdata", "malawi.geojson")
  survey <- file.path("testdata", "survey.csv")
  art <- file.path("testdata", "programme.csv")
  anc <- file.path("testdata", "anc.csv")

  mock_build_json <- mockery::mock('"{"test"}')
  with_mock("hintr:::build_json" = mock_build_json,  {
    json <- do_endpoint_model_options(shape, survey, art, anc)
    args <- mockery::mock_args(mock_build_json)
    expect_length(args[[1]], 2)
    expect_true(grepl('"label": "ART"', args[[1]][[1]]))
    expect_true(grepl('"label": "ANC"', args[[1]][[1]]))
    params <- args[[1]][[2]]
    expect_equal(names(params),
      c("area_scope_options", "area_scope_default", "area_level_options",
        "t1_options", "t2_options", "survey_prevalence_options",
        "survey_art_coverage_options", "survey_vls_options",
        "survey_recently_infected_options", "survey_art_or_vls_options",
        "art_t1_options", "art_t2_options", "anc_prevalence_t1_options",
        "anc_prevalence_t2_options", "anc_art_coverage_t1_options",
        "anc_art_coverage_t2_options"))

    expect_length(params$area_scope_options, 1)
    expect_equal(names(params$area_scope_options[[1]]),
                 c("id", "label", "children"))
    expect_equal(params$area_scope_options[[1]]$id, scalar("MWI"))
    expect_equal(params$area_scope_options[[1]]$label, scalar("Malawi"))
    expect_length(params$area_scope_options[[1]]$children, 3)
    expect_equal(params$area_scope_default$id, scalar("MWI"))
    expect_equal(params$area_scope_default$label, scalar("Malawi"))
    expect_equal(params$area_level_options, list(
      list(
        id = scalar(0),
        label = scalar("Country")
      ),
      list(
        id = scalar(1),
        label = scalar("Region")
      ),
      list(
        id = scalar(2),
        label = scalar("Zone")
      ),
      list(
        id = scalar(3),
        label = scalar("District")
      ),
      list(
        id = scalar(4),
        label = scalar("District + Metro")
      )))
    expect_equal(params$t1_options[[length(params$t1_options)]]$id,
                 scalar("449"))
    expect_equal(params$t1_options[[length(params$t1_options)]]$label,
                 scalar("Jan-Mar 2012"))
    expect_true(length(params$t1_options) >= 32)
    expect_equal(params$t1_options[[length(params$t2_options)]]$id,
                 scalar("449"))
    expect_equal(params$t1_options[[length(params$t2_options)]]$label,
                 scalar("Jan-Mar 2012"))
    expect_true(length(params$t2_options) >= 32)
    expect_length(params$survey_prevalence_options, 4)
    expect_equal(params$survey_prevalence_options[[1]]$id,
                 scalar("MWI2016PHIA"))
    expect_equal(params$survey_prevalence_options[[1]]$label,
                 scalar("MWI2016PHIA"))
    expect_equal(params$survey_prevalence_options,
                 params$survey_art_coverage_options)
    expect_equal(params$survey_prevalence_options,
                 params$survey_vls_options)
    expect_equal(params$survey_prevalence_options,
                 params$survey_recently_infected_options)
    expect_equal(params$survey_art_or_vls_options[[1]]$id,
                 scalar("art_coverage"))
    expect_equal(params$survey_art_or_vls_options[[2]]$id, scalar("vls"))
    expect_length(params$art_t1_options, 32)
    expect_equal(params$art_t1_options[[1]]$id, scalar("445"))
    expect_equal(params$art_t1_options[[1]]$label, scalar("Jan-Mar 2011"))
    expect_equal(params$art_t2_options, params$art_t1_options)
    expect_length(params$anc_prevalence_t1_options, 29)
    expect_equal(params$anc_prevalence_t1_options[[1]]$id, scalar("447"))
    expect_equal(params$anc_prevalence_t1_options[[1]]$label,
                 scalar("Jul-Sep 2011"))
    expect_equal(params$anc_prevalence_t1_options,
                 params$anc_prevalence_t2_options)
    expect_equal(params$anc_prevalence_t1_options,
                 params$anc_art_coverage_t1_options)
    expect_equal(params$anc_prevalence_t1_options,
                 params$anc_art_coverage_t2_options)
  })
})

test_that("do_endpoint_model_options without programme data", {
  shape <- file.path("testdata", "malawi.geojson")
  survey <- file.path("testdata", "survey.csv")

  mock_build_json <- mockery::mock('"{"test"}')
  with_mock("hintr:::build_json" = mock_build_json,  {
    json <- do_endpoint_model_options(shape, survey, NULL, NULL)
    args <- mockery::mock_args(mock_build_json)
    expect_length(args[[1]], 2)
    expect_false(grepl('"label": "ART"', args[[1]][[1]]))
    expect_false(grepl('"label": "ANC"', args[[1]][[1]]))
    params <- args[[1]][[2]]
    expect_equal(names(params),
      c("area_scope_options", "area_scope_default", "area_level_options",
        "t1_options", "t2_options", "survey_prevalence_options",
        "survey_art_coverage_options", "survey_vls_options",
        "survey_recently_infected_options", "survey_art_or_vls_options",
        "art_t1_options", "art_t2_options", "anc_prevalence_t1_options",
        "anc_prevalence_t2_options", "anc_art_coverage_t1_options",
        "anc_art_coverage_t2_options"))

    expect_length(params$area_scope_options, 1)
    expect_equal(names(params$area_scope_options[[1]]),
                 c("id", "label", "children"))
    expect_equal(params$area_scope_options[[1]]$id, scalar("MWI"))
    expect_equal(params$area_scope_options[[1]]$label, scalar("Malawi"))
    expect_length(params$area_scope_options[[1]]$children, 3)
    expect_equal(params$area_scope_default$id, scalar("MWI"))
    expect_equal(params$area_scope_default$label, scalar("Malawi"))
    expect_equal(params$area_level_options, list(
      list(
        id = scalar(0),
        label = scalar("Country")
      ),
      list(
        id = scalar(1),
        label = scalar("Region")
      ),
      list(
        id = scalar(2),
        label = scalar("Zone")
      ),
      list(
        id = scalar(3),
        label = scalar("District")
      ),
      list(
        id = scalar(4),
        label = scalar("District + Metro")
      )))
    expect_equal(params$t1_options[[length(params$t1_options)]]$id,
                 scalar("449"))
    expect_equal(params$t1_options[[length(params$t1_options)]]$label,
                 scalar("Jan-Mar 2012"))
    expect_true(length(params$t1_options) >= 32)
    expect_equal(params$t1_options[[length(params$t1_options)]]$id,
                 scalar("449"))
    expect_equal(params$t1_options[[length(params$t2_options)]]$label,
                 scalar("Jan-Mar 2012"))
    expect_true(length(params$t2_options) >= 32)
    expect_length(params$survey_prevalence_options, 4)
    expect_equal(params$survey_prevalence_options[[1]]$id,
                 scalar("MWI2016PHIA"))
    expect_equal(params$survey_prevalence_options[[1]]$label,
                 scalar("MWI2016PHIA"))
    expect_equal(params$survey_prevalence_options,
                 params$survey_art_coverage_options)
    expect_equal(params$survey_prevalence_options,
                 params$survey_vls_options)
    expect_equal(params$survey_prevalence_options,
                 params$survey_recently_infected_options)
    expect_equal(params$survey_art_or_vls_options[[1]]$id,
                 scalar("art_coverage"))
    expect_equal(params$survey_art_or_vls_options[[2]]$id, scalar("vls"))
    expect_null(params$art_t1_options)
    expect_null(params$art_t2_options)
    expect_null(params$anc_prevalence_t1_options)
    expect_null(params$anc_prevalence_t2_options)
    expect_null(params$anc_art_coverage_t1_options)
    expect_null(params$anc_art_coverage_t2_options)
  })
})

test_that("can retrieve validated model options", {
  shape <- file.path("testdata", "malawi.geojson")
  survey <- file.path("testdata", "survey.csv")
  art <- file.path("testdata", "programme.csv")
  anc <- file.path("testdata", "anc.csv")
  json <- do_endpoint_model_options(shape, survey, art, anc)

  json <- jsonlite::parse_json(json)
  expect_equal(names(json), "controlSections")
  expect_length(json$controlSections, 4)

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
    names(general_section$controlGroups[[1]]$controls[[1]]$value),
    c("id", "label"))
  expect_equal(
    general_section$controlGroups[[1]]$controls[[1]]$value$id,
    "MWI")
  expect_equal(
    general_section$controlGroups[[1]]$controls[[1]]$value$label,
    "Malawi")
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
    0)
  expect_equal(
    general_section$controlGroups[[2]]$controls[[1]]$options[[1]]$label,
    "Country")

  survey_section <- json$controlSections[[2]]
  expect_length(
    survey_section$controlGroups[[1]]$controls[[1]]$options,
    4
  )
  expect_equal(
    names(survey_section$controlGroups[[1]]$controls[[1]]$options[[1]]),
    c("id", "label"))
  expect_equal(
    survey_section$controlGroups[[1]]$controls[[1]]$options[[1]]$id,
    "MWI2016PHIA")
  expect_equal(
    survey_section$controlGroups[[1]]$controls[[1]]$options[[1]]$label,
    "MWI2016PHIA")

  art_section <- json$controlSections[[3]]
  expect_length(
    art_section$controlGroups[[1]]$controls[[1]]$options,
    32
  )
  expect_equal(
    names(art_section$controlGroups[[1]]$controls[[1]]$options[[1]]),
    c("id", "label"))
  expect_equal(
    art_section$controlGroups[[1]]$controls[[1]]$options[[1]]$id,
    "445")
  expect_equal(
    art_section$controlGroups[[1]]$controls[[1]]$options[[1]]$label,
    "Jan-Mar 2011")
  expect_equal(
    names(art_section$controlGroups[[1]]$controls[[2]]$options[[1]]),
    c("id", "label"))
  expect_equal(
    art_section$controlGroups[[1]]$controls[[2]]$options[[1]]$id,
    "445")
  expect_equal(
    art_section$controlGroups[[1]]$controls[[2]]$options[[1]]$label,
    "Jan-Mar 2011")

  anc_section <- json$controlSections[[4]]
  expect_length(
    anc_section$controlGroups[[1]]$controls[[1]]$options,
    29
  )
  expect_equal(
    names(anc_section$controlGroups[[1]]$controls[[1]]$options[[1]]),
    c("id", "label"))
  expect_equal(
    anc_section$controlGroups[[1]]$controls[[1]]$options[[1]]$id,
    "447")
  expect_equal(
    anc_section$controlGroups[[1]]$controls[[1]]$options[[1]]$label,
    "Jul-Sep 2011")
})


test_that("can read geojson level labels", {
  shape <- file.path("testdata", "malawi.geojson")
  json <- hintr_geojson_read(shape)
  levels <- get_level_options(json)
  expect_length(levels, 5)
  expect_equal(levels, list(
    list(
      id = scalar(0),
      label = scalar("Country")
    ),
    list(
      id = scalar(1),
      label = scalar("Region")
    ),
    list(
      id = scalar(2),
      label = scalar("Zone")
    ),
    list(
      id = scalar(3),
      label = scalar("District")
    ),
    list(
      id = scalar(4),
      label = scalar("District + Metro")
    )))
})
