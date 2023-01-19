test_that("endpoint_model_options returns model options", {
  input <- model_options_input(file.path("testdata", "malawi.geojson"),
                               file.path("testdata", "survey.csv"),
                               file.path("testdata", "programme.csv"),
                               file.path("testdata", "anc.csv"))
  response <- model_options(input)
  json <- jsonlite::parse_json(response)

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
    length(survey_section$controlGroups[[1]]$controls[[1]]$options) >=
      32
  )
  expect_length(
    survey_section$controlGroups[[2]]$controls[[1]]$options,
    4
  )
  expect_equal(
    names(survey_section$controlGroups[[2]]$controls[[1]]$options[[1]]),
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

})

test_that("endpoint_model_options can be run without programme data", {
  input <- model_options_input(file.path("testdata", "malawi.geojson"),
                               file.path("testdata", "survey.csv"),
                               NULL,
                               NULL)

  response <- model_options(input)
  json <- jsonlite::parse_json(response)

  expect_equal(names(json), "controlSections")
  expect_length(json$controlSections, 4)

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
  expect_length(
    survey_section$controlGroups[[2]]$controls[[1]]$options,
    4
  )
  expect_equal(
    names(survey_section$controlGroups[[2]]$controls[[1]]$options[[1]]),
    c("id", "label"))
  expect_equal(
    survey_section$controlGroups[[2]]$controls[[1]]$options[[1]]$id,
    "DEMO2016PHIA")
  expect_equal(
    survey_section$controlGroups[[2]]$controls[[1]]$options[[1]]$label,
    "DEMO2016PHIA")
})

test_that("endpoint_model_options fails without shape & survey data", {
  input <- model_options_input(NULL,
                               NULL,
                               file.path("testdata", "programme.csv"),
                               file.path("testdata", "anc.csv"))

  error <- expect_error(model_options(input))

  expect_equal(error$data[[1]]$error, scalar("INVALID_OPTIONS"))
  expect_equal(
    error$data[[1]]$detail,
    scalar("File at path NULL does not exist. Create it, or fix the path."))
  expect_equal(error$status_code, 400)
})

test_that("endpoint_model_options_validate validates options", {
  input <- '{
    "data": {
      "pjnz": "path/to/pjnz",
      "shape": "path",
      "population": "path",
      "survey": "path",
      "programme": "path",
      "anc": "path"
    },
    "options": {
      "option1": "true"
    }
  }'

  mock_validate_model_options <- mockery::mock(list(
    valid = TRUE,
    warnings = list(
      list(
        text = "a warning",
        locations = list("location")
      )
    )
  ))
  with_mock(validate_model_options = mock_validate_model_options, {
    response <- model_options_validate(input)
  })

  expect_equal(names(response), c("valid", "warnings"))
  expect_equal(response$valid, scalar(TRUE))
  expect_equal(response$warnings[[1]]$text, scalar("a warning"))
  expect_equal(response$warnings[[1]]$locations[[1]], "location")
})

test_that("invalid model options returns error", {
  input <- '{
    "data": {
      "pjnz": "path/to/pjnz",
      "shape": "path",
      "population": "path",
      "survey": "path",
      "programme": "path",
      "anc": "path"
    },
    "options": {
      "option1": "true"
    }
  }'

  mock_validate_model_options <- mockery::mock(stop("Invalid options"))
  with_mock(validate_model_options = mock_validate_model_options, {
    error <- expect_error(model_options_validate(input))
  })

  expect_equal(error$data[[1]]$error, scalar("INVALID_OPTIONS"))
  expect_equal(error$data[[1]]$detail, scalar("Invalid options"))
  expect_equal(error$status_code, 400)
})

test_that("can get calibration options", {
  options <- calibration_options("MWI")
  expect_length(options, 1)
  expect_true(any(grepl("Calibration options", options)))
  expect_true(any(grepl("controlSections", options)))
})

test_that("failing to get calibration options throws hintr error", {
  mock_calibration_options <- mockery::mock(stop("Failed to get options"))
  with_mock(get_controls_json = mock_calibration_options, {
    error <- expect_error(calibration_options("MWI"))
  })
  expect_equal(error$data[[1]]$error, scalar("INVALID_CALIBRATION_OPTIONS"))
  expect_equal(error$data[[1]]$detail, scalar("Failed to get options"))
  expect_equal(error$status_code, 400)
})
