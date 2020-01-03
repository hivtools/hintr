context("endpoints-model-options")

gc()

test_that("endpoint_model_options returns model options", {
  res <- MockPlumberResponse$new()
  shape <- file.path("testdata", "malawi.geojson")
  survey <- file.path("testdata", "survey.csv")
  programme <- file.path("testdata", "programme.csv")
  anc <- file.path("testdata", "anc.csv")
  shape_file <- list(path = shape, hash = "12345", filename = "original")
  survey_file <- list(path = survey, hash = "12345", filename = "original")
  programme_file <- list(path = programme, hash = "12345", filename = "original")
  anc_file <- list(path = anc, hash = "12345", filename = "original")

  response <- endpoint_model_options(NULL, res, shape_file, survey_file,
                                     programme_file, anc_file)
  json <- jsonlite::parse_json(response)

  expect_equal(res$status, 200)
  expect_equal(names(json$data), "controlSections")
  expect_length(json$data$controlSections, 7)

  general_section <- json$data$controlSections[[1]]
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

  survey_section <- json$data$controlSections[[2]]
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
    "MWI2016PHIA")
  expect_equal(
    survey_section$controlGroups[[2]]$controls[[1]]$options[[1]]$label,
    "MWI2016PHIA")

  anc_section <- json$data$controlSections[[3]]
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

  art_section <- json$data$controlSections[[4]]
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
  res <- MockPlumberResponse$new()
  shape <- file.path("testdata", "malawi.geojson")
  survey <- file.path("testdata", "survey.csv")
  shape_file <- list(path = shape, hash = "12345", filename = "original")
  survey_file <- list(path = survey, hash = "12345", filename = "original")

  response <- endpoint_model_options(NULL, res, shape_file, survey_file)
  json <- jsonlite::parse_json(response)

  expect_equal(res$status, 200)
  expect_equal(names(json$data), "controlSections")
  expect_length(json$data$controlSections, 5)

  general_section <- json$data$controlSections[[1]]
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

  survey_section <- json$data$controlSections[[2]]
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
    "MWI2016PHIA")
  expect_equal(
    survey_section$controlGroups[[2]]$controls[[1]]$options[[1]]$label,
    "MWI2016PHIA")
})

test_that("endpoint_model_options fails without shape & survey data", {
  res <- MockPlumberResponse$new()
  programme <- file.path("testdata", "programme.csv")
  anc <- file.path("testdata", "anc.csv")
  programme_file <- list(path = programme, hash = "12345", filename = "original")
  anc_file <- list(path = anc, hash = "12345", filename = "original")

  response <- endpoint_model_options(NULL, res, NULL, NULL, programme_file,
                                     anc_file)
  json <- jsonlite::parse_json(response)

  expect_equal(res$status, 400)
  expect_equal(json$status, "failure")
  expect_equal(json$errors[[1]]$error, "INVALID_OPTIONS")
  expect_equal(json$errors[[1]]$detail,
               "File at path NULL does not exist. Create it, or fix the path.")
})

test_that("endpoint_model_options_validate validates options", {
  skip("Skipping model option validation endpoint not implemented mrc-592")

  data <- list(
    pjnz = "path/to/pjnz",
    shape = "path",
    population = "path",
    survey = "path",
    programme = "path",
    anc = "path"
  )
  options = list(
    option1 = "true"
  )
  req <- list(postBody = "")

  res <- MockPlumberResponse$new()
  response <- endpoint_model_options_validate(req, res, data, options)
  response <- jsonlite::parse_json(response)

  expect_equal(res$status, 200)
  expect_equal(response$status, "success")
  expect_length(response$errors, 0)
  expect_equal(names(response$data), "valid")
  expect_equal(response$data$valid, TRUE)
})

test_that("invalid model options returns error", {
  skip("Skipping model option validation endpoint not implemented mrc-592")

  data <- list(
    pjnz = "path/to/pjnz",
    shape = "path",
    population = "path",
    survey = "path",
    programme = "path",
    anc = "path"
  )
  options = list(
    option1 = "true"
  )
  req <- list(postBody = "")

  res <- MockPlumberResponse$new()
  mock_validate_model_options <- mockery::mock(stop("Invalid options"))
  with_mock("naomi::validate_model_options" = mock_validate_model_options, {
    response <- endpoint_model_options_validate(req, res, data, options)
  })
  response <- jsonlite::parse_json(response)

  expect_equal(res$status, 400)
  expect_equal(response$status, "failure")
  expect_length(response$data, 0)
  expect_length(response$errors, 1)
  expect_equal(names(response$errors[[1]]), c("error", "detail", "key"))
  expect_equal(response$errors[[1]]$error, "INVALID_OPTIONS")
  expect_equal(response$errors[[1]]$detail, "Invalid options")
})
