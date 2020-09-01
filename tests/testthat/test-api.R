context("api")

test_that("don't change language if not asked to", {
  data <- new.env()
  tr <- hintr_translator()
  expect_equal(tr$language(), "en")

  res <- MockPlumberResponse$new()
  req <- list()
  value <- list("the response")

  expect_null(api_set_language(data, req, res))
  expect_equal(tr$language(), "en")
  expect_identical(api_reset_language(data, req, res, value), value)
  expect_equal(tr$language(), "en")
})

test_that("change language based on header", {
  data <- new.env()

  tr_hintr <- hintr_translator()
  tr_hintr$set_language("en")
  expect_equal(tr_hintr$language(), "en")

  tr_naomi <- traduire::translator(package = "naomi")
  tr_naomi$set_language("en")
  expect_equal(tr_naomi$language(), "en")

  res <- MockPlumberResponse$new()
  req <- list(HEADERS = c("accept-language" = "fr"))
  value <- list("the response")

  expect_null(api_set_language(data, req, res))
  expect_equal(tr_hintr$language(), "fr")
  expect_equal(tr_naomi$language(), "fr")

  expect_identical(api_reset_language(data, req, res, value), value)
  expect_equal(tr_hintr$language(), "en")
  expect_equal(tr_naomi$language(), "en")
})

test_that("endpoint_baseline_individual", {
  endpoint <- endpoint_baseline_individual()
  response <- endpoint$run(readLines("payload/validate_pjnz_payload.json"))

  expect_equal(response$status_code, 200)
  expect_null(response$error)
  expect_equal(response$data$hash, scalar("12345"))
  expect_equal(response$data$data$country, scalar("Malawi"))
  expect_equal(response$data$data$iso3, scalar("MWI"))
  expect_equal(response$data$filename, scalar("Malawi2019.PJNZ"))
  expect_equal(response$data$filters, json_null())
})

test_that("endpoint_baseline_individual works", {
  test_redis_available()
  queue <- test_queue(workers = 0)
  api <- api_build(queue)
  res <- api$request("POST", "/validate/baseline-individual",
                     body = readLines("payload/validate_pjnz_payload.json"))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$data$hash, "12345")
  expect_equal(body$data$data$country, "Malawi")
  expect_equal(body$data$data$iso3, "MWI")
  expect_equal(body$data$filename, "Malawi2019.PJNZ")
  expect_equal(body$data$filters, NULL)
})

test_that("endpoint_baseline_combined", {
  endpoint <- endpoint_baseline_combined()
  response <- endpoint$run(readLines("payload/validate_baseline_payload.json"))

  expect_equal(response$status_code, 200)
  expect_null(response$error)
  expect_equal(response$data$consistent, scalar(TRUE))
})

test_that("endpoint_baseline_combined works", {
  test_redis_available()
  queue <- test_queue(workers = 0)
  api <- api_build(queue)
  res <- api$request("POST", "/validate/baseline-combined",
                     body = readLines("payload/validate_baseline_payload.json"))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$status, "success")
  expect_null(body$errors)
  expect_true(body$data$consistent)
})

test_that("endpoint_validate_survey_programme programme", {
  endpoint <- endpoint_validate_survey_programme()
  response <- endpoint$run(readLines("payload/validate_programme_payload.json"))

  expect_equal(response$status_code, 200)
  expect_null(response$error)
  expect_equal(response$data$filename, scalar("original.csv"))
  expect_equal(response$data$hash, scalar("12345"))
  ## Sanity check that data has been returned
  expect_true(nrow(response$data$data) >= 200)
  expect_equal(typeof(response$data$data[, "current_art"]), "double")
})

test_that("endpoint_validate_survey_programme works with programme data", {
  test_redis_available()
  queue <- test_queue(workers = 0)
  api <- api_build(queue)
  res <- api$request("POST", "/validate/survey-and-programme",
                     body =
                       readLines("payload/validate_programme_payload.json"))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$status, "success")
  expect_null(body$errors)
  expect_equal(body$data$filename, "original.csv")
  expect_equal(body$data$hash, "12345")
  ## Sanity check that data has been returned
  expect_true(nrow(body$data$data) >= 200)
  expect_equal(typeof(body$data$data[, "current_art"]), "double")
})

test_that("endpoint_validate_survey_programme anc", {
  endpoint <- endpoint_validate_survey_programme()
  response <- endpoint$run(readLines("payload/validate_anc_payload.json"))

  expect_equal(response$status_code, 200)
  expect_null(response$error)
  expect_equal(response$data$filename, scalar("original.csv"))
  expect_equal(response$data$hash, scalar("12345"))
  ## Sanity check that data has been returned
  expect_true(nrow(response$data$data) >= 200)
  expect_equal(typeof(response$data$data[, "prevalence"]), "double")
  expect_equal(typeof(response$data$data[, "art_coverage"]), "double")
})

test_that("endpoint_validate_survey_programme works with anc data", {
  test_redis_available()
  queue <- test_queue(workers = 0)
  api <- api_build(queue)
  res <- api$request("POST", "/validate/survey-and-programme",
                     body = readLines("payload/validate_anc_payload.json"))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$status, "success")
  expect_null(body$errors)
  expect_equal(body$data$filename, "original.csv")
  expect_equal(body$data$hash, "12345")
  ## Sanity check that data has been returned
  expect_true(nrow(body$data$data) >= 200)
  expect_equal(typeof(body$data$data[, "prevalence"]), "double")
  expect_equal(typeof(body$data$data[, "art_coverage"]), "double")
})

test_that("endpoint_validate_survey_programme survey", {
  endpoint <- endpoint_validate_survey_programme()
  response <- endpoint$run(readLines("payload/validate_survey_payload.json"))

  expect_equal(response$status_code, 200)
  expect_null(response$error)
  expect_equal(response$data$filename, scalar("original.csv"))
  expect_equal(response$data$hash, scalar("12345"))
  ## Sanity check that data has been returned
  expect_true(nrow(response$data$data) >= 20000)
  expect_equal(typeof(response$data$data[, "est"]), "double")
})

test_that("endpoint_validate_survey_programme works with survey data", {
  test_redis_available()
  queue <- test_queue(workers = 0)
  api <- api_build(queue)
  res <- api$request("POST", "/validate/survey-and-programme",
                     body = readLines("payload/validate_survey_payload.json"))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$status, "success")
  expect_null(body$errors)
  expect_equal(body$data$filename, "original.csv")
  expect_equal(body$data$hash, "12345")
  ## Sanity check that data has been returned
  expect_true(nrow(body$data$data) >= 20000)
  expect_equal(typeof(body$data$data[, "est"]), "double")
})

test_that("endpoint_model_options", {
  endpoint <- endpoint_model_options()
  response <- endpoint$run(readLines("payload/model_run_options_payload.json"))

  expect_equal(response$status_code, 200)
  expect_null(response$error)
  body <- jsonlite::parse_json(response$body)
  expect_equal(names(body$data), "controlSections")
  expect_length(body$data$controlSections, 7)

  general_section <- body$data$controlSections[[1]]
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

  survey_section <- body$data$controlSections[[2]]
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

  anc_section <- body$data$controlSections[[3]]
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

  art_section <- body$data$controlSections[[4]]
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

  expect_true(!is.null(body$version))
  expect_equal(names(body$version), c("hintr", "naomi", "rrq", "traduire"))
  expect_true(all(grepl("^(\\d+\\.)?(\\d+\\.)?(\\*|\\d+)$", body$version)))
})

test_that("endpoint_model_options works", {
  test_redis_available()
  queue <- test_queue(workers = 0)
  api <- api_build(queue)
  res <- api$request("POST", "/model/options",
                     body = readLines("payload/model_run_options_payload.json"))
  expect_equal(res$status, 200)
  body <- jsonlite::parse_json(res$body)
  expect_equal(names(body$data), "controlSections")
  expect_length(body$data$controlSections, 7)

  general_section <- body$data$controlSections[[1]]
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

  survey_section <- body$data$controlSections[[2]]
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

  anc_section <- body$data$controlSections[[3]]
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

  art_section <- body$data$controlSections[[4]]
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

  expect_true(!is.null(body$version))
  expect_equal(names(body$version), c("hintr", "naomi", "rrq", "traduire"))
  expect_true(all(grepl("^(\\d+\\.)?(\\d+\\.)?(\\*|\\d+)$", body$version)))
})

test_that("endpoint_model_options_validate can be run", {
  test_redis_available()
  queue <- test_queue()

  endpoint <- endpoint_model_options_validate()
  response <- endpoint$run(readLines("payload/validate_options_payload.json"))

  expect_equal(response$status_code, 200)
  expect_null(response$error)
  expect_equal(response$data$valid, scalar(TRUE))
})

test_that("api can call endpoint_model_options_validate", {
  test_redis_available()
  queue <- test_queue()
  api <- api_build(queue)
  res <- api$request("POST", "/validate/options",
                     body = readLines("payload/validate_options_payload.json"))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$status, "success")
  expect_null(body$errors)
  expect_true(body$data$valid)
})

test_that("endpoint_model_submit can be run", {
  test_redis_available()
  queue <- test_queue()
  endpoint <- endpoint_model_submit(queue)
  path <- setup_submit_payload()
  response <- endpoint$run(readLines(path))

  expect_equal(response$status_code, 200)
  expect_null(response$error)
  expect_true(!is.null(response$data$id))
})

test_that("api can call endpoint_model_submit", {
  test_redis_available()
  queue <- test_queue()
  api <- api_build(queue)
  path <- setup_submit_payload()
  res <- api$request("POST", "/model/submit",
                     body = readLines(path))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$status, "success")
  expect_null(body$errors)
  expect_true(!is.null(body$data$id))
})

test_that("endpoint_model_status can be run", {
  test_redis_available()
  test_mock_model_available()
  queue <- test_queue(workers = 1)
  model_run <- endpoint_model_submit(queue)
  path <- setup_submit_payload()
  run_response <- model_run$run(readLines(path))
  expect_equal(run_response$status_code, 200)
  expect_true(!is.null(run_response$data$id))

  endpoint <- endpoint_model_status(queue)
  out <- queue$queue$task_wait(run_response$data$id)
  response <- endpoint$run(run_response$data$id)
  expect_equal(response$status_code, 200)
  expect_equal(response$data$id, run_response$data$id)
  expect_equal(response$data$done, scalar(TRUE))
  expect_equal(response$data$status, scalar("COMPLETE"))
  expect_equal(response$data$queue, scalar(0))
  expect_equal(response$data$success, scalar(TRUE))
  expect_length(response$data$progress, 2)
  expect_equal(response$data$progress[[1]]$name, scalar("Started mock model"))
  expect_true(response$data$progress[[1]]$complete)
  expect_equal(response$data$progress[[2]]$name, scalar("Finished mock model"))
  expect_false(response$data$progress[[2]]$complete)
})

test_that("api can call endpoint_model_status", {
  test_redis_available()
  test_mock_model_available()
  queue <- test_queue(workers = 1)
  api <- api_build(queue)
  path <- setup_submit_payload()
  res <- api$request("POST", "/model/submit",
                     body = readLines(path))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$status, "success")
  expect_true(!is.null(body$data$id))

  out <- queue$queue$task_wait(body$data$id)
  res <- api$request("GET", sprintf("/model/status/%s", body$data$id))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$status, "success")
  expect_null(body$errors)
  expect_equal(body$data$id, body$data$id)
  expect_equal(body$data$done, TRUE)
  expect_equal(body$data$status, "COMPLETE")
  expect_equal(body$data$queue, 0)
  expect_equal(body$data$success, TRUE)
  expect_equal(nrow(body$data$progress), 2)
  expect_equal(body$data$progress[1, "name"], "Started mock model")
  expect_true(body$data$progress[1, "complete"])
  expect_equal(body$data$progress[2, "name"], "Finished mock model")
  expect_false(body$data$progress[2, "complete"])
})

test_that("endpoint_model_result can be run", {
  test_redis_available()
  test_mock_model_available()
  queue <- test_queue(workers = 1)
  model_run <- endpoint_model_submit(queue)
  path <- setup_submit_payload()
  run_response <- model_run$run(readLines(path))
  expect_equal(run_response$status_code, 200)
  expect_true(!is.null(run_response$data$id))

  endpoint <- endpoint_model_result(queue)
  out <- queue$queue$task_wait(run_response$data$id)
  response <- endpoint$run(run_response$data$id)

  expect_equal(response$status_code, 200)
  expect_equal(names(response$data), c("data", "plottingMetadata"))
  expect_equal(colnames(response$data$data),
               c("area_id", "sex", "age_group", "calendar_quarter",
                 "indicator_id", "mode", "mean", "lower", "upper"))
  expect_true(nrow(response$data$data) > 84042)
  expect_equal(names(response$data$plottingMetadata),
               c("barchart", "choropleth"))
})

test_that("api can call endpoint_model_result", {
  test_redis_available()
  test_mock_model_available()
  queue <- test_queue(workers = 1)
  api <- api_build(queue)
  path <- setup_submit_payload()
  res <- api$request("POST", "/model/submit",
                     body = readLines(path))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$status, "success")
  expect_true(!is.null(body$data$id))

  out <- queue$queue$task_wait(body$data$id)
  res <- api$request("GET", sprintf("/model/result/%s", body$data$id))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)

  expect_equal(body$status, "success")
  expect_null(body$errors)
  expect_equal(names(body$data), c("data", "plottingMetadata"))
  expect_equal(colnames(body$data$data),
               c("area_id", "sex", "age_group", "calendar_quarter",
                 "indicator_id", "mode", "mean", "lower", "upper"))
  expect_true(nrow(body$data$data) > 84042)
  expect_equal(names(body$data$plottingMetadata),
               c("barchart", "choropleth"))
})

test_that("endpoint_model_cancel can be run", {
  test_redis_available()
  test_mock_model_available()
  queue <- test_queue(workers = 1)
  model_run <- endpoint_model_submit(queue)
  path <- setup_submit_payload()
  run_response <- model_run$run(readLines(path))
  expect_equal(run_response$status_code, 200)
  expect_true(!is.null(run_response$data$id))

  endpoint <- endpoint_model_cancel(queue)
  response <- endpoint$run(run_response$data$id)

  expect_equal(response$status_code, 200)
  expect_equal(response$data, json_null())
})

test_that("api can call endpoint_model_cancel", {
  test_redis_available()
  test_mock_model_available()
  queue <- test_queue(workers = 1)
  api <- api_build(queue)
  path <- setup_submit_payload()
  res <- api$request("POST", "/model/submit",
                     body = readLines(path))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$status, "success")
  expect_true(!is.null(body$data$id))

  res <- api$request("POST", sprintf("/model/cancel/%s", body$data$id))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)

  expect_equal(body$status, "success")
  expect_null(body$errors)
  expect_null(body$data)
})

test_that("erroring model run returns useful messages", {
  test_redis_available()

  queue <- MockQueue$new()
  api <- api_build(queue)
  path <- setup_submit_payload()
  res <- api$request("POST", "/model/submit",
                     body = readLines(path))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$status, "success")
  expect_true(!is.null(body$data$id))

  mock_id <- mockery::mock(scalar("fake_key"), cycle = TRUE)
  with_mock("ids::proquint" = mock_id, {
    res <- api$request("GET", sprintf("/model/result/%s", body$data$id))
  })
  expect_equal(res$status, 400)
  body <- jsonlite::fromJSON(res$body)

  expect_equal(body$status, "failure")
  expect_length(body$data, 0)
  expect_true(nrow(body$errors) == 1)
  expect_equal(body$errors[1, "error"], "MODEL_RUN_FAILED")
  expect_equal(body$errors[1, "detail"], "test error")
  expect_equal(body$errors[1, "key"], "fake_key")
  expect_true("rrq:::rrq_worker_main()" %in% body$errors[1, "trace"][[1]])

  ## Check logging:
  msg <- capture_messages(
    hintr:::api_log_end(NULL, NULL, res, NULL))
  expect_match(msg[[1]], "error-key: fake_key")
  expect_match(msg[[2]], "error-detail: test error")
  expect_match(msg[[3]], "error-trace: rrq:::rrq_worker_main")
})

test_that("endpoint_plotting_metadata can be run", {
  endpoint <- endpoint_plotting_metadata()
  response <- endpoint$run("MWI")

  expect_equal(response$status_code, 200)
  expect_null(response$error)
  expect_true(all(names(response$data) %in%
                    c("survey", "anc", "output", "programme")))
})

test_that("api can call endpoint_plotting_metadata", {
  test_redis_available()
  queue <- test_queue(workers = 0)
  api <- api_build(queue)
  res <- api$request("GET", "/meta/plotting/MWI")
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$status, "success")
  expect_null(body$errors)
  expect_true(all(names(body$data) %in%
                    c("survey", "anc", "output", "programme")))
})

test_that("returning_json_version adds version", {
  returning_with_version <- returning_json_version(
    "ValidateInputResponse.schema", schema_root())
  returning <- pkgapi::pkgapi_returning_json(
    "ValidateInputResponse.schema", schema_root())
  input <- list(
    hash = scalar("12345"),
    filename = scalar("original"),
    type = scalar("population"),
    data = json_null(),
    filters = json_null()
  )
  version_out <- returning_with_version$process(input)
  out <- returning$process(input)
  version_response <- jsonlite::parse_json(version_out)
  response <- jsonlite::parse_json(out)
  expect_equal(names(version_response),
               c("status", "errors", "data", "version"))
  expect_equal(version_response$status, response$status)
  expect_equal(version_response$errors, response$errors)
  expect_equal(version_response$data, response$data)
  expect_equal(version_response$version$hintr,
               unclass(cfg$version_info$hintr))
  expect_equal(version_response$version$naomi,
               unclass(cfg$version_info$naomi))
  expect_equal(version_response$version$rrq,
               unclass(cfg$version_info$rrq))
  expect_equal(version_response$version$traduire,
               unclass(cfg$version_info$traduire))
})

test_that("endpoint_download_spectrum can be run", {
  test_redis_available()
  test_mock_model_available()

  queue <- test_queue(workers = 1)
  run_endpoint <- endpoint_model_submit(queue)
  path <- setup_submit_payload()
  run_response <- run_endpoint$run(readLines(path))
  expect_equal(run_response$status_code, 200)
  out <- queue$queue$task_wait(run_response$data$id)

  endpoint <- endpoint_download_spectrum(queue)
  response <- endpoint$run(run_response$data$id)

  expect_equal(response$status_code, 200)
  expect_match(response$headers$`Content-Disposition`,
               'attachment; filename="MWI_\\d+-\\d+_naomi_spectrum_digest.zip"')
  ## Size of bytes is close to expected
  size <- length(response$data)
  expect_true(size - size/10 <
                file.size(system.file("output", "malawi_spectrum_download.zip",
                                      package = "hintr")))
  expect_true(size + size/10 >
                file.size(system.file("output", "malawi_spectrum_download.zip",
                                      package = "hintr")))
})

test_that("api can call endpoint_download_spectrum", {
  test_redis_available()
  test_mock_model_available()

  queue <- test_queue(workers = 1)
  api <- api_build(queue)

  ## Run the model
  path <- setup_submit_payload()
  res <- api$request("POST", "/model/submit",
                     body = readLines(path))
  expect_equal(res$status, 200)
  response <- jsonlite::fromJSON(res$body)
  out <- queue$queue$task_wait(response$data$id)

  ## Get result
  res <- api$request("GET", paste0("/download/spectrum/", response$data$id))

  expect_equal(res$status, 200)
  expect_equal(res$headers$`Content-Type`, "application/octet-stream")
  expect_match(res$headers$`Content-Disposition`,
               'attachment; filename="MWI_\\d+-\\d+_naomi_spectrum_digest.zip"')
  ## Size of bytes is close to expected
  size <- length(res$body)
  expect_true(size - size/10 <
                file.size(system.file("output", "malawi_spectrum_download.zip",
                                      package = "hintr")))
  expect_true(size + size/10 >
                file.size(system.file("output", "malawi_spectrum_download.zip",
                                      package = "hintr")))
})

test_that("endpoint_download_summary can be run", {
  test_redis_available()
  test_mock_model_available()

  queue <- test_queue(workers = 1)
  run_endpoint <- endpoint_model_submit(queue)
  path <- setup_submit_payload()
  run_response <- run_endpoint$run(readLines(path))
  expect_equal(run_response$status_code, 200)
  out <- queue$queue$task_wait(run_response$data$id)

  endpoint <- endpoint_download_summary(queue)
  response <- endpoint$run(run_response$data$id)

  expect_equal(response$status_code, 200)
  expect_match(
    response$headers$`Content-Disposition`,
    'attachment; filename="MWI_\\d+-\\d+_naomi_coarse_age_groups.zip"')
  ## Size of bytes is close to expected
  size <- length(response$data)
  expect_true(size - size/10 <
                file.size(system.file("output", "malawi_summary_download.zip",
                                      package = "hintr")))
  expect_true(size + size/10 >
                file.size(system.file("output", "malawi_summary_download.zip",
                                      package = "hintr")))
})

test_that("api can call endpoint_download_summary", {
  test_redis_available()
  test_mock_model_available()

  queue <- test_queue(workers = 1)
  api <- api_build(queue)

  ## Run the model
  path <- setup_submit_payload()
  res <- api$request("POST", "/model/submit",
                     body = readLines(path))
  expect_equal(res$status, 200)
  response <- jsonlite::fromJSON(res$body)
  out <- queue$queue$task_wait(response$data$id)

  ## Get result
  res <- api$request("GET", paste0("/download/summary/", response$data$id))

  expect_equal(res$status, 200)
  expect_equal(res$headers$`Content-Type`, "application/octet-stream")
  expect_match(
    res$headers$`Content-Disposition`,
    'attachment; filename="MWI_\\d+-\\d+_naomi_coarse_age_groups.zip"')
  ## Size of bytes is close to expected
  size <- length(res$body)
  expect_true(size - size/10 <
                file.size(system.file("output", "malawi_summary_download.zip",
                                      package = "hintr")))
  expect_true(size + size/10 >
                file.size(system.file("output", "malawi_summary_download.zip",
                                      package = "hintr")))
})

test_that("returning_binary_head ensures no body in response", {
  returning <- returning_binary_head()
  data <- charToRaw("test")
  expect_null(returning$process(data))
  expect_true(returning$validate(NULL))
})

test_that("endpoint_download_spectrum_head returns headers only", {
  test_redis_available()
  test_mock_model_available()

  queue <- test_queue(workers = 1)
  run_endpoint <- endpoint_model_submit(queue)
  path <- setup_submit_payload()
  run_response <- run_endpoint$run(readLines(path))
  expect_equal(run_response$status_code, 200)
  out <- queue$queue$task_wait(run_response$data$id)

  endpoint <- endpoint_download_spectrum_head(queue)
  response <- endpoint$run(run_response$data$id)

  expect_equal(response$status_code, 200)
  expect_equal(response$content_type, "application/octet-stream")
  expect_match(response$headers$`Content-Disposition`,
               'attachment; filename="MWI_\\d+-\\d+_naomi_spectrum_digest.zip"')
  expect_null(response$body, NULL)
})

test_that("api endpoint_download_spectrum_head returns headers only", {
  test_redis_available()
  test_mock_model_available()

  queue <- test_queue(workers = 1)
  api <- api_build(queue)

  ## Run the model
  path <- setup_submit_payload()
  res <- api$request("POST", "/model/submit",
                     body = readLines(path))
  expect_equal(res$status, 200)
  response <- jsonlite::fromJSON(res$body)
  out <- queue$queue$task_wait(response$data$id)

  ## Get result
  res <- api$request("HEAD", paste0("/download/spectrum/", response$data$id))

  expect_equal(res$status, 200)
  expect_equal(res$headers$`Content-Type`, "application/octet-stream")
  expect_match(res$headers$`Content-Disposition`,
               'attachment; filename="MWI_\\d+-\\d+_naomi_spectrum_digest.zip"')
  ## Plumber uses an empty string to represent an empty body
  expect_equal(res$body, "")
})

test_that("endpoint_download_summary_head returns headers only", {
  test_redis_available()
  test_mock_model_available()

  queue <- test_queue(workers = 1)
  run_endpoint <- endpoint_model_submit(queue)
  path <- setup_submit_payload()
  run_response <- run_endpoint$run(readLines(path))
  expect_equal(run_response$status_code, 200)
  out <- queue$queue$task_wait(run_response$data$id)

  endpoint <- endpoint_download_summary_head(queue)
  response <- endpoint$run(run_response$data$id)

  expect_equal(response$status_code, 200)
  expect_equal(response$content_type, "application/octet-stream")
  expect_match(
    response$headers$`Content-Disposition`,
    'attachment; filename="MWI_\\d+-\\d+_naomi_coarse_age_groups.zip"')
  expect_null(response$body, NULL)
})

test_that("api endpoint_download_summary_head returns headers only", {
  test_redis_available()
  test_mock_model_available()

  queue <- test_queue(workers = 1)
  api <- api_build(queue)

  ## Run the model
  path <- setup_submit_payload()
  res <- api$request("POST", "/model/submit",
                     body = readLines(path))
  expect_equal(res$status, 200)
  response <- jsonlite::fromJSON(res$body)
  out <- queue$queue$task_wait(response$data$id)

  ## Get result
  res <- api$request("HEAD", paste0("/download/summary/", response$data$id))

  expect_equal(res$status, 200)
  expect_equal(res$headers$`Content-Type`, "application/octet-stream")
  expect_match(
    res$headers$`Content-Disposition`,
    'attachment; filename="MWI_\\d+-\\d+_naomi_coarse_age_groups.zip"')
  ## Plumber uses an empty string to represent an empty body
  expect_equal(res$body, "")
})

test_that("endpoint_model_debug can be run", {
  test_redis_available()
  test_mock_model_available()

  queue <- test_queue(workers = 1)
  run_endpoint <- endpoint_model_submit(queue)
  path <- setup_submit_payload()
  run_response <- run_endpoint$run(readLines(path))
  expect_equal(run_response$status_code, 200)
  out <- queue$queue$task_wait(run_response$data$id)

  endpoint <- endpoint_model_debug(queue)
  response <- endpoint$run(run_response$data$id)

  expect_equal(response$status_code, 200)
  expect_match(response$headers$`Content-Disposition`,
               'attachment; filename="\\w+_\\d+-\\d+_naomi_debug.zip"')
  ## Download contains data
  expect_true(length(response$data) > 1000000)
})

test_that("api can call endpoint_model_debug", {
  test_redis_available()
  test_mock_model_available()

  queue <- test_queue(workers = 1)
  api <- api_build(queue)

  ## Run the model
  path <- setup_submit_payload()
  res <- api$request("POST", "/model/submit",
                     body = readLines(path))
  expect_equal(res$status, 200)
  response <- jsonlite::fromJSON(res$body)
  out <- queue$queue$task_wait(response$data$id)

  ## Get result
  res <- api$request("GET", paste0("/model/debug/", response$data$id))

  expect_equal(res$status, 200)
  expect_equal(res$headers$`Content-Type`, "application/octet-stream")
  expect_match(res$headers$`Content-Disposition`,
               'attachment; filename="\\w+_\\d+-\\d+_naomi_debug.zip"')
  ## Download contains data
  expect_true(length(res$body) > 1000000)
})

test_that("endpoint_hintr_version works", {
  endpoint <- endpoint_hintr_version()
  response <- endpoint$run()

  expect_is(response$data, "list")
  expect_setequal(names(response$data), c("hintr", "naomi", "rrq", "traduire"))
  expect_equal(response$data$rrq, scalar(as.character(packageVersion("rrq"))))
})

test_that("api can call endpoint_hintr_version", {
  test_redis_available()

  queue <- test_queue()
  api <- api_build(queue)
  res <- api$request("GET", "/hintr/version")
  expect_equal(res$status, 200)
  response <- jsonlite::fromJSON(res$body)

  expect_is(response$data, "list")
  expect_setequal(names(response$data), c("hintr", "naomi", "rrq", "traduire"))
  expect_equal(response$data$rrq, as.character(packageVersion("rrq")))
})

test_that("endpoint_hintr_worker_status works", {
  test_redis_available()

  queue <- test_queue(workers = 2)
  endpoint <- endpoint_hintr_worker_status(queue)
  response <- endpoint$run()

  expect_equal(unlist(response$data, FALSE, FALSE), rep("IDLE", 2))
})

test_that("api can call endpoint_hintr_worker_status", {
  test_redis_available()

  queue <- test_queue(workers = 2)
  api <- api_build(queue)
  res <- api$request("GET", "/hintr/worker/status")
  expect_equal(res$status, 200)
  response <- jsonlite::fromJSON(res$body)

  expect_equal(unlist(response$data, FALSE, FALSE), rep("IDLE", 2))
})

test_that("endpoint_hintr_stop works", {
  test_redis_available()

  queue <- test_queue()
  mock_hintr_stop <- mockery::mock(function() NULL)
  mockery::stub(endpoint_hintr_stop, "hintr_stop", mock_hintr_stop)
  endpoint <- endpoint_hintr_stop(queue)
  response <- endpoint$run()

  mockery::expect_called(mock_hintr_stop, 1)
})

test_that("api can call endpoint_hintr_stop", {
  test_redis_available()

  queue <- test_queue()
  mock_hintr_stop <- mockery::mock(function() NULL)
  with_mock("hintr:::hintr_stop" = mock_hintr_stop, {
    api <- api_build(queue)
    res <- api$request("POST", "/hintr/stop")
  })
  expect_equal(res$status, 200)
  mockery::expect_called(mock_hintr_stop, 1)
})

test_that("404 errors have sensible schema", {
  test_redis_available()

  queue <- test_queue()
  api <- api_build(queue)
  res <- api$request("GET", "/meaning-of-life")

  expect_equal(res$status, 404)
  response <- jsonlite::fromJSON(res$body)
  expect_equal(response$status, "failure")
  expect_equal(response$errors[1, "error"], "NOT_FOUND")
  expect_equal(response$errors[1, "detail"],
               "GET /meaning-of-life is not a valid hintr path")
  expect_equal(response$data, setNames(list(), list()))
})
