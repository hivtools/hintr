context("model-options")

test_that("can build JSON from template", {
  json <- build_json("test <param1>", list(param1 = "test string"))
  expect_equal(json, 'test "test string"')

  json <- build_json("<param1> test, <param_2>",
                     list(param1 = "x", param_2 = "y"))
  expect_equal(json, '"x" test, "y"')

  json <- build_json('{"options": [<options>], "test": <test>}',
                     list(options = c("MWI", "MWI.1", "MWI.2"),
                          test = "test_value"))
  expect_equal(json,
               '{"options": ["MWI", "MWI.1", "MWI.2"], "test": "test_value"}')

  ## Additional params are ignored
  json <- build_json("test <param>", list(param = "test", param2 = "test2"))
  expect_equal(json, 'test "test"')

  ## Null params
  json <- build_json('{"options": <param>}', list(param = NULL))
  expect_equal(json, '{"options": ""}')
})

test_that("JSON build fails if params are missing", {
  expect_error(build_json('{"options": <param>}', list(value = "test")),
               "Failed to construct model options from template and params:\n\\w+")
})

test_that("do_endpoint_model_options correctly builds params list", {
  model_options <- naomi::get_model_options_template()
  shape <- file.path("testdata", "malawi.geojson")
  survey <- file.path("testdata", "survey.csv")
  art <- file.path("testdata", "programme.csv")
  anc <- file.path("testdata", "anc.csv")

  mock_build_json <- mockery::mock('"{"test"}')
  with_mock("hintr:::build_json" = mock_build_json,  {
    json <- do_endpoint_model_options(model_options, shape, survey, art, anc)
    args <- mockery::mock_args(mock_build_json)
    expect_length(args[[1]], 2)
    expect_equal(args[[1]][[1]], model_options)
    params <- args[[1]][[2]]
    expect_equal(names(params), c("area_scope_options", "area_scope_default",
                                     "area_level_options", "art_t1_options",
                                     "art_t2_options"))

    expect_equal(params$area_scope_options[1:5],
                 c("MWI", "MWI.1", "MWI.2", "MWI.3", "MWI.1.1"))
    expect_equal(params$area_scope_default, "MWI")
    expect_equal(params$area_level_options,
                 c("Country", "Region", "Zone", "District", "District + Metro"))
    expect_equal(params$art_t1_options[1:3],
                 c("Jan-Mar 2011", "Apr-Jun 2011", "Jul-Sep 2011"))
    expect_equal(params$art_t2_options[1:3],
                 c("Jan-Mar 2011", "Apr-Jun 2011", "Jul-Sep 2011"))
  })
})

test_that("can retrieve validated model options", {
  model_options <- naomi::get_model_options_template()
  shape <- file.path("testdata", "malawi.geojson")
  survey <- file.path("testdata", "survey.csv")
  art <- file.path("testdata", "programme.csv")
  anc <- file.path("testdata", "anc.csv")
  json <- do_endpoint_model_options(model_options, shape, survey, art, anc)

  json <- jsonlite::parse_json(json)
  expect_equal(names(json), "controlSections")
  expect_length(json$controlSections, 3)
  ## Check some options have been added
  expect_equal(
    json$controlSections[[1]]$controlGroups[[1]]$controls[[1]]$options[[1]],
    "MWI")
  expect_equal(
    json$controlSections[[1]]$controlGroups[[1]]$controls[[1]]$default,
    "MWI")
  expect_equal(
    json$controlSections[[1]]$controlGroups[[2]]$controls[[1]]$options[[1]],
    "Country")
  expect_equal(
    json$controlSections[[2]]$controlGroups[[1]]$controls[[1]]$options[[1]],
    "Jan-Mar 2011")
  expect_equal(
    json$controlSections[[2]]$controlGroups[[1]]$controls[[2]]$options[[1]],
    "Jan-Mar 2011")
})
