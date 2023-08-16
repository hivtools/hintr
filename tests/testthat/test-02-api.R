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

  tr_naomi_options <- traduire::translator(package = "naomi.options")
  tr_naomi_options$set_language("en")
  expect_equal(tr_naomi_options$language(), "en")

  res <- MockPlumberResponse$new()
  req <- list(HEADERS = c("accept-language" = "fr"))
  value <- list("the response")

  expect_null(api_set_language(data, req, res))
  expect_equal(tr_hintr$language(), "fr")
  expect_equal(tr_naomi$language(), "fr")
  expect_equal(tr_naomi_options$language(), "fr")

  expect_identical(api_reset_language(data, req, res, value), value)
  expect_equal(tr_hintr$language(), "en")
  expect_equal(tr_naomi$language(), "en")
  expect_equal(tr_naomi_options$language(), "en")
})

test_that("can build api", {
  test_redis_available()
  api <- api(workers = 0)
  expect_s3_class(api, "porcelain")
  out <- capture_output(res <- api$request("GET", "/"))
  expect_match(out, "request GET /")
  expect_match(out, "response GET /")
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$data, "Welcome to hintr")
})

test_that("endpoint_baseline_individual", {
  endpoint <- endpoint_baseline_individual()
  response <- endpoint$run(system_file("payload", "validate_pjnz_payload.json"))
  expect_equal(response$status_code, 200)
  expect_null(response$error)
  expect_equal(response$data$hash, scalar("12345"))
  expect_equal(response$data$data$country, scalar("Malawi"))
  expect_equal(response$data$data$iso3, scalar("MWI"))
  expect_equal(response$data$filename, scalar("Malawi2019.PJNZ"))
  expect_equal(response$data$filters, json_null())
  expect_equal(response$data$fromADR, scalar(FALSE))
  expect_equal(response$data$resource_url,
               scalar("https://adr.unaids.org/file/123.csv"))
})

test_that("endpoint_baseline_individual works", {
  test_redis_available()
  queue <- test_queue(workers = 0)
  api <- api_build(queue)
  res <- api$request("POST", "/validate/baseline-individual",
                     body = system_file("payload", "validate_pjnz_payload.json"))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$data$hash, "12345")
  expect_equal(body$data$data$country, "Malawi")
  expect_equal(body$data$data$iso3, "MWI")
  expect_equal(body$data$filename, "Malawi2019.PJNZ")
  expect_equal(body$data$filters, NULL)
  expect_equal(body$data$fromADR, FALSE)
  expect_equal(body$data$resource_url, "https://adr.unaids.org/file/123.csv")
})

test_that("endpoint_baseline_combined", {
  endpoint <- endpoint_baseline_combined()
  response <- endpoint$run(system_file("payload",
                                       "validate_baseline_payload.json"))

  expect_equal(response$status_code, 200)
  expect_null(response$error)
  expect_equal(response$data$consistent, scalar(TRUE))
})

test_that("endpoint_baseline_combined works", {
  test_redis_available()
  queue <- test_queue(workers = 0)
  api <- api_build(queue)
  res <- api$request(
    "POST", "/validate/baseline-combined",
    body = system_file("payload", "validate_baseline_payload.json"))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$status, "success")
  expect_null(body$errors)
  expect_true(body$data$consistent)
})

test_that("endpoint_model_options", {
  endpoint <- endpoint_model_options()
  response <- endpoint$run(
    system_file("payload", "model_run_options_payload.json"))

  expect_equal(response$status_code, 200)
  expect_null(response$error)
  body <- jsonlite::parse_json(response$body)
  expect_equal(names(body$data), "controlSections")
  expect_length(body$data$controlSections, 6)

  general_section <- body$data$controlSections[[1]]
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
    "DEMO2016PHIA")
  expect_equal(
    survey_section$controlGroups[[2]]$controls[[1]]$options[[1]]$label,
    "DEMO2016PHIA")

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
  res <- api$request(
    "POST", "/model/options",
    body = system_file("payload", "model_run_options_payload.json"))
  expect_equal(res$status, 200)
  body <- jsonlite::parse_json(res$body)
  expect_equal(names(body$data), "controlSections")
  expect_length(body$data$controlSections, 6)

  general_section <- body$data$controlSections[[1]]
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
    "DEMO2016PHIA")
  expect_equal(
    survey_section$controlGroups[[2]]$controls[[1]]$options[[1]]$label,
    "DEMO2016PHIA")

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
  response <- endpoint$run(
    system_file("payload", "validate_options_payload.json"))

  expect_equal(response$status_code, 200)
  expect_null(response$error)
  expect_equal(response$data$valid, scalar(TRUE))
  expect_equal(response$data$warnings, list(list(
    text = scalar(paste0("You have chosen to fit model without estimating ",
                         "neighbouring ART attendance. You may wish to review ",
                         "your selection to include this option.")),
    locations = "model_options")
  ))
})

test_that("api can call endpoint_model_options_validate", {
  test_redis_available()
  queue <- test_queue()
  api <- api_build(queue)
  res <- api$request(
    "POST", "/validate/options",
    body = system_file("payload", "validate_options_payload.json"))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body, simplifyVector = FALSE)
  expect_equal(body$status, "success")
  expect_null(body$errors)
  expect_true(body$data$valid)
  expect_equal(body$data$warnings, list(list(
    text = paste0("You have chosen to fit model without estimating ",
                  "neighbouring ART attendance. You may wish to review ",
                  "your selection to include this option."),
    locations = list("model_options"))
  ))
})

test_that("endpoint_model_submit can be run", {
  test_redis_available()
  queue <- test_queue(workers = 0)
  endpoint <- endpoint_model_submit(queue)
  payload <- setup_payload_submit()
  response <- endpoint$run(payload)

  expect_equal(response$status_code, 200)
  expect_null(response$error)
  expect_true(!is.null(response$data$id))
})

test_that("api can call endpoint_model_submit", {
  test_redis_available()
  queue <- test_queue()
  api <- api_build(queue)
  payload <- setup_payload_submit()
  res <- api$request("POST", "/model/submit", body = payload)

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
  payload <- setup_payload_submit()
  run_response <- model_run$run(payload)

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
  expect_equal(response$data$progress[[2]]$helpText, scalar("model running"))
})

test_that("api can call endpoint_model_status", {
  test_redis_available()
  test_mock_model_available()
  queue <- test_queue(workers = 1)
  api <- api_build(queue)
  payload <- setup_payload_submit()
  res <- api$request("POST", "/model/submit", body = payload)

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
  payload <- setup_payload_submit()
  run_response <- model_run$run(payload)

  expect_equal(run_response$status_code, 200)
  expect_true(!is.null(run_response$data$id))

  endpoint <- endpoint_model_result(queue)
  out <- queue$queue$task_wait(run_response$data$id)
  response <- endpoint$run(run_response$data$id)

  expect_equal(response$status_code, 200)
  expect_equal(response$data, list(
    id = scalar(run_response$data$id),
    complete = scalar(TRUE),
    warnings = list(
      list(text = scalar(paste0("Zero population input for 8 population ",
                                "groups. Replaced with population 0.1.")),
           locations = list(scalar("model_fit"))
      ))
  ))
})

test_that("api can call endpoint_model_result", {
  test_redis_available()
  test_mock_model_available()
  queue <- test_queue(workers = 1)
  api <- api_build(queue)
  payload <- setup_payload_submit()
  res <- api$request("POST", "/model/submit", body = payload)

  expect_equal(res$status, 200)
  submit_body <- jsonlite::fromJSON(res$body)
  expect_equal(submit_body$status, "success")
  expect_true(!is.null(submit_body$data$id))

  out <- queue$queue$task_wait(submit_body$data$id)
  res <- api$request("GET", sprintf("/model/result/%s", submit_body$data$id))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body,  simplifyVector = FALSE)

  expect_equal(body$status, "success")
  expect_null(body$errors)
  expect_equal(body$data, list(
    id = submit_body$data$id,
    complete = TRUE,
    warnings = list(
      list(text = paste0("Zero population input for 8 population ",
                                "groups. Replaced with population 0.1."),
           locations = list("model_fit")
      ))
  ))
})

test_that("endpoint_model_cancel can be run", {
  test_redis_available()
  test_mock_model_available()
  queue <- test_queue(workers = 1)
  model_run <- endpoint_model_submit(queue)
  payload <- setup_payload_submit()
  run_response <- model_run$run(payload)

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
  payload <- setup_payload_submit()
  res <- api$request("POST", "/model/submit", body = payload)

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
  payload <- setup_payload_submit()
  res <- api$request("POST", "/model/submit", body = payload)

  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$status, "success")
  expect_true(!is.null(body$data$id))

  out <- queue$queue$task_wait(body$data$id)
  res <- api$request("GET", sprintf("/model/result/%s", body$data$id))
  expect_equal(res$status, 400)
  body <- jsonlite::fromJSON(res$body)

  expect_equal(body$status, "failure")
  expect_length(body$data, 0)
  expect_true(nrow(body$errors) == 1)
  expect_equal(body$errors[1, "error"], "MODEL_RUN_FAILED")
  expect_equal(body$errors[1, "detail"], "test error")
  expect_match(body$errors[1, "key"], "\\w+-\\w+-\\w+")
  expect_match(body$errors[1, "job_id"][[1]], "^[[:xdigit:]]+$")
})

test_that("endpoint_model_calibrate_options", {
  endpoint <- endpoint_model_calibrate_options()
  response <- endpoint$run("MWI")

  expect_equal(response$status_code, 200)
  expect_null(response$error)
  body <- jsonlite::parse_json(response$body)
  expect_equal(names(body$data), "controlSections")
  expect_true(length(body$data$controlSections) >= 1)

  expect_true(!is.null(body$version))
  expect_equal(names(body$version), c("hintr", "naomi", "rrq", "traduire"))
  expect_true(all(grepl("^(\\d+\\.)?(\\d+\\.)?(\\*|\\d+)$", body$version)))
})

test_that("endpoint_calibrate_options works", {
  test_redis_available()
  queue <- test_queue(workers = 0)
  api <- api_build(queue)
  res <- api$request("POST", "/calibrate/options/MWI")
  expect_equal(res$status, 200)
  body <- jsonlite::parse_json(res$body)
  expect_equal(names(body$data), "controlSections")
  expect_true(length(body$data$controlSections) >= 1)

  expect_true(!is.null(body$version))
  expect_equal(names(body$version), c("hintr", "naomi", "rrq", "traduire"))
  expect_true(all(grepl("^(\\d+\\.)?(\\d+\\.)?(\\*|\\d+)$", body$version)))
})

test_that("endpoint_plotting_metadata_iso3 can be run", {
  endpoint <- endpoint_plotting_metadata_iso3()
  response <- endpoint$run("MWI")

  expect_equal(response$status_code, 200)
  expect_null(response$error)
  expect_true(all(names(response$data) %in%
                    c("survey", "anc", "output", "programme")))
})

test_that("endpoint_plotting_metadata_default can be run", {
  endpoint <- endpoint_plotting_metadata_default()
  response <- endpoint$run()

  expect_equal(response$status_code, 200)
  expect_null(response$error)
  expect_true(all(names(response$data) %in%
                    c("survey", "anc", "output", "programme")))
})

test_that("api can call endpoint_plotting_metadata with iso3", {
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

test_that("api can call endpoint_plotting_metadata without iso3", {
  test_redis_available()
  queue <- test_queue(workers = 0)
  api <- api_build(queue)
  res <- api$request("GET", "/meta/plotting")
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
  returning <- porcelain::porcelain_returning_json(
    "ValidateInputResponse.schema", schema_root())
  input <- list(
    hash = scalar("12345"),
    filename = scalar("original"),
    type = scalar("population"),
    data = json_null(),
    filters = json_null(),
    fromADR = FALSE
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

test_that("content disposition header is formatted correctly", {
  expect_match(build_content_disp_header("MWI", "naomi-output", ".zip"),
               'attachment; filename="MWI_naomi-output_\\d+-\\d+.zip"')
  expect_match(build_content_disp_header(NULL, "naomi-output", ".zip"),
               'attachment; filename="naomi-output_\\d+-\\d+.zip"')
  expect_match(
    build_content_disp_header(c("MWI.1", "MWI.2"), "naomi-output", ".zip"),
    'attachment; filename="MWI.1_MWI.2_naomi-output_\\d+-\\d+.zip"')
})

test_that("returning_binary_head ensures no body in response", {
  returning <- returning_binary_head()
  data <- charToRaw("test")
  expect_null(returning$process(data))
  expect_true(returning$validate(NULL))
})

test_that("endpoint_model_debug can be run", {
  test_redis_available()
  test_mock_model_available()

  queue <- test_queue(workers = 1)
  run_endpoint <- endpoint_model_submit(queue)
  payload <- setup_payload_submit()
  run_response <- run_endpoint$run(payload)

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
  payload <- setup_payload_submit()
  res <- api$request("POST", "/model/submit", body = payload)

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

  expect_type(response$data, "list")
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

  expect_type(response$data, "list")
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
  with_mock(hintr_stop = mock_hintr_stop, {
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

test_that("model calibrate can be queued and result returned", {
  test_mock_model_available()
  q <- test_queue_result()

  ## Submit calibrate request
  submit <- endpoint_model_calibrate_submit(q$queue)
  payload <- setup_payload_calibrate()
  submit_response <- submit$run(q$model_run_id, payload)

  expect_equal(submit_response$status_code, 200)
  expect_true(!is.null(submit_response$data$id))

  ## Status
  out <- q$queue$queue$task_wait(submit_response$data$id)
  status <- endpoint_model_calibrate_status(q$queue)
  status_response <- status$run(submit_response$data$id)

  expect_equal(status_response$status_code, 200)
  expect_equal(status_response$data$id, submit_response$data$id)
  expect_true(status_response$data$done)
  expect_equal(status_response$data$status, scalar("COMPLETE"))
  expect_true(status_response$data$success)
  expect_equal(status_response$data$queue, scalar(0))
  expect_match(status_response$data$progress[[1]],
               "Saving outputs - [\\d.m\\s]+s elapsed", perl = TRUE)

  ## Get result
  result <- endpoint_model_calibrate_result(q$queue)
  response <- result$run(status_response$data$id)

  expect_equal(response$status_code, 200)
  expect_equal(names(response$data), c("data", "plottingMetadata", "warnings"))
  expect_equal(colnames(response$data$data),
               c("area_id", "sex", "age_group", "calendar_quarter",
                 "indicator", "mode", "mean", "lower", "upper"))
  expect_true(nrow(response$data$data) > 84042)
  expect_equal(names(response$data$plottingMetadata),
               c("barchart", "choropleth"))

  ## Get metadata
  metadata <- endpoint_model_calibrate_metadata(q$queue)
  metadata_response <- metadata$run(status_response$data$id)
  expect_equal(metadata_response$data$plottingMetadata,
               response$data$plottingMetadata)
  expect_equal(metadata_response$data$warnings,
               response$data$warnings)

  ## Get data
  data <- endpoint_model_calibrate_data(q$queue)
  data_response <- data$run(status_response$data$id)
  expect_equal(data_response$data$data,
               response$data$data)
})

test_that("api can call endpoint_model_calibrate", {
  test_mock_model_available()
  q <- test_queue_result()

  ## Submit calibrate
  api <- api_build(q$queue)
  calibrate_payload <- setup_payload_calibrate()
  submit_res <- api$request("POST",
                            paste0("/calibrate/submit/", q$model_run_id),
                            body = calibrate_payload)

  expect_equal(submit_res$status, 200)
  submit_body <- jsonlite::fromJSON(submit_res$body)
  expect_true(!is.null(submit_body$data$id))

  ## Status
  out <- q$queue$queue$task_wait(submit_body$data$id)
  status_res <- api$request("GET",
                            paste0("/calibrate/status/", submit_body$data$id))

  expect_equal(status_res$status, 200)
  status_body <- jsonlite::fromJSON(status_res$body)
  expect_equal(status_body$data$id, submit_body$data$id)
  expect_true(status_body$data$done)
  expect_equal(status_body$data$status, "COMPLETE")
  expect_true(status_body$data$success)
  expect_equal(status_body$data$queue, 0)
  expect_match(status_body$data$progress[[1]],
               "Saving outputs - [\\d.m\\s]+s elapsed", perl = TRUE)

  ## Get result
  result_res <- api$request("GET",
                            paste0("/calibrate/result/", status_body$data$id))

  expect_equal(result_res$status, 200)
  result_body <- jsonlite::fromJSON(result_res$body)
  expect_null(result_body$errors)
  expect_equal(names(result_body$data),
               c("data", "plottingMetadata", "warnings"))
  expect_equal(colnames(result_body$data$data),
               c("area_id", "sex", "age_group", "calendar_quarter",
                 "indicator", "mode", "mean", "lower", "upper"))
  expect_true(nrow(result_body$data$data) > 84042)
  expect_equal(names(result_body$data$plottingMetadata),
               c("barchart", "choropleth"))

  ## Get metadata
  metadata_res <- api$request("GET", paste0("/calibrate/result/metadata/",
                                            status_body$data$id))
  expect_equal(metadata_res$status, 200)
  metadata_body <- jsonlite::fromJSON(metadata_res$body)
  expect_equal(metadata_body$data$plottingMetadata,
               result_body$data$plottingMetadata)
  expect_equal(metadata_body$data$warnings,
               result_body$data$warnings)

  ## Get data
  data_res <- api$request("GET", paste0("/calibrate/result/data/",
                                        status_body$data$id))
  expect_equal(data_res$status, 200)
  data_body <- jsonlite::fromJSON(data_res$body)
  expect_equal(data_body$data$data,
               result_body$data$data)
})

test_that("model calibrate result includes warnings", {
  test_mock_model_available()
  q <- test_queue_result()

  result <- endpoint_model_calibrate_result(q$queue)
  response <- result$run(q$calibrate_id)

  expect_equal(response$status_code, 200)
  expect_length(response$data$warnings, 2)
  expect_equal(response$data$warnings[[1]]$text,
               scalar("ART coverage greater than 100% for 10 age groups"))
  expect_equal(response$data$warnings[[1]]$locations, "model_calibrate")
  expect_equal(response$data$warnings[[2]]$text,
               scalar("Prevalence greater than 40%"))
  expect_equal(response$data$warnings[[2]]$locations,
               c("model_calibrate", "review_output"))
})

test_that("can get calibrate plot data", {
  test_mock_model_available()
  test_redis_available()
  q <- test_queue_result()

  endpoint <- endpoint_model_calibrate_plot(q$queue)
  response <- endpoint$run(q$calibrate_id)

  expect_equal(response$status_code, 200)
  response_data <- response$data
  expect_setequal(names(response_data), c("data", "plottingMetadata"))
  expect_setequal(names(response_data$data),
                  c("data_type", "spectrum_region_code", "spectrum_region_name",
                    "sex", "age_group", "calendar_quarter", "indicator",
                    "mean"))
  expect_true(nrow(response_data$data) > 0)
  expect_equal(names(response_data$plottingMetadata), "barchart")
  expect_setequal(names(response_data$plottingMetadata$barchart),
                  c("indicators", "filters", "defaults"))

  expect_setequal(names(response_data$plottingMetadata$barchart$indicators),
                  c("indicator", "value_column", "error_low_column",
                    "error_high_column", "indicator_column", "indicator_value",
                    "indicator_sort_order", "name", "scale", "accuracy",
                    "format"))
  expect_true(nrow(response_data$plottingMetadata$barchart$indicators) > 0)

  filters <- lapply(response_data$plottingMetadata$barchart$filters, "[[",
                    "column_id")
  expect_equal(filters[[1]], scalar("spectrum_region_code"))
  expect_equal(filters[[2]], scalar("calendar_quarter"))
  expect_equal(filters[[3]], scalar("sex"))
  expect_equal(filters[[4]], scalar("age_group"))
  expect_equal(filters[[5]], scalar("data_type"))

  expect_setequal(names(response_data$plottingMetadata$barchart$defaults),
                  c("indicator_id", "x_axis_id", "disaggregate_by_id",
                    "selected_filter_options"))
})

test_that("API can return calibration plotting data", {
  test_redis_available()
  test_mock_model_available()
  q <- test_queue_result()

  api <- api_build(q$queue)
  res <- api$request("GET", paste0("/calibrate/plot/", q$calibrate_id))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body, simplifyDataFrame = FALSE)
  expect_equal(body$status, "success")
  expect_null(body$errors)

  response_data <- body$data
  expect_setequal(names(response_data), c("data", "plottingMetadata"))
  data <- do.call(rbind, response_data$data)
  expect_setequal(colnames(data),
                  c("data_type", "spectrum_region_code", "spectrum_region_name",
                    "sex", "age_group", "calendar_quarter", "indicator",
                    "mean"))
  expect_true(nrow(data) > 0)
  expect_equal(names(response_data$plottingMetadata), "barchart")
  expect_setequal(names(response_data$plottingMetadata$barchart),
                  c("indicators", "filters", "defaults"))

  barchart_indicators <- do.call(
    rbind, response_data$plottingMetadata$barchart$indicators)
  expect_setequal(colnames(barchart_indicators),
                  c("indicator", "value_column", "error_low_column",
                    "error_high_column", "indicator_column", "indicator_value",
                    "indicator_sort_order", "name", "scale", "accuracy",
                    "format"))
  expect_true(nrow(barchart_indicators) > 0)

  filters <- lapply(response_data$plottingMetadata$barchart$filters, "[[",
                    "column_id")
  expect_equal(filters[[1]], "spectrum_region_code")
  expect_equal(filters[[2]], "calendar_quarter")
  expect_equal(filters[[3]], "sex")
  expect_equal(filters[[4]], "age_group")
  expect_equal(filters[[5]], "data_type")

  expect_setequal(names(response_data$plottingMetadata$barchart$defaults),
                  c("indicator_id", "x_axis_id", "disaggregate_by_id",
                    "selected_filter_options"))
})

test_that("error returned from calibrate_plot for old model output", {
  test_mock_model_available()
  test_redis_available()
  q <- test_queue_result(calibrate = mock_calibrate_v1.0.7)

  endpoint <- endpoint_model_calibrate_plot(q$queue)
  response <- endpoint$run(q$calibrate_id)

  expect_equal(response$status_code, 500)
  expect_equal(response$value$errors[[1]]$detail, scalar(
               "Model output out of date please re-run model and try again."))
})

test_that("calibrate plot metadata is translated", {
  test_mock_model_available()
  test_redis_available()
  q <- test_queue_result()

  response <- with_hintr_language("fr", {
    endpoint <- endpoint_model_calibrate_plot(q$queue)
    response <- endpoint$run(q$calibrate_id)
  })

  expect_equal(response$status_code, 200)

  filters <- response$data$plottingMetadata$barchart$filters
  expect_equal(filters[[1]]$label, scalar("Zone"))
  expect_equal(filters[[1]]$options[[1]]$label, scalar("Malawi"))
  expect_equal(filters[[2]]$label, scalar("Période"))
  expect_equal(filters[[2]]$options[[1]]$label, scalar("Juin 2019"))
  expect_equal(filters[[3]]$label, scalar("Sexe"))
  expect_equal(filters[[3]]$options[[1]]$label, scalar("Both"))
  expect_equal(filters[[4]]$label, scalar("Âge"))
  expect_equal(filters[[4]]$options[[1]]$label, scalar("15-49"))
  expect_equal(filters[[5]]$label, scalar("Type de données"))
  expect_equal(filters[[5]]$options[[2]]$label, scalar("Étalonné"))
})

test_that("can get comparison plot data", {
  test_mock_model_available()
  test_redis_available()
  q <- test_queue_result()

  endpoint <- endpoint_comparison_plot(q$queue)
  response <- endpoint$run(q$calibrate_id)

  expect_equal(response$status_code, 200)
  response_data <- response$data
  expect_setequal(names(response_data), c("data", "plottingMetadata"))
  expect_setequal(names(response_data$data),
                  c("area_id", "area_name", "age_group", "sex",
                    "calendar_quarter", "indicator", "source", "mean",
                    "lower", "upper"))
  expect_true(nrow(response_data$data) > 0)
  expect_equal(names(response_data$plottingMetadata), "barchart")
  expect_setequal(names(response_data$plottingMetadata$barchart),
                  c("indicators", "filters", "defaults", "selections"))

  expect_setequal(names(response_data$plottingMetadata$barchart$indicators),
                  c("indicator", "value_column", "error_low_column",
                    "error_high_column", "indicator_column", "indicator_value",
                    "indicator_sort_order", "name", "scale", "accuracy",
                    "format"))
  expect_true(nrow(response_data$plottingMetadata$barchart$indicators) > 0)

  filters <- lapply(response_data$plottingMetadata$barchart$filters, "[[",
                    "column_id")
  expect_equal(filters[[1]], scalar("area_id"))
  expect_equal(filters[[2]], scalar("calendar_quarter"))
  expect_equal(filters[[3]], scalar("sex"))
  expect_equal(filters[[4]], scalar("age_group"))
  expect_equal(filters[[5]], scalar("source"))

  expect_setequal(names(response_data$plottingMetadata$barchart$defaults),
                  c("indicator_id", "x_axis_id", "disaggregate_by_id",
                    "selected_filter_options"))

  expect_true(length(response_data$plottingMetadata$barchart$selections) >= 5)
  for (selection in response_data$plottingMetadata$barchart$selections) {
    expect_setequal(names(selection),
                    c("indicator_id", "x_axis_id", "disaggregate_by_id",
                      "selected_filter_options"))
  }
})

test_that("API can return comparison plotting data", {
  test_redis_available()
  test_mock_model_available()
  q <- test_queue_result()

  api <- api_build(q$queue)
  res <- api$request("GET", paste0("/comparison/plot/", q$calibrate_id))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body, simplifyDataFrame = FALSE)
  expect_equal(body$status, "success")
  expect_null(body$errors)

  response_data <- body$data
  expect_setequal(names(response_data), c("data", "plottingMetadata"))
  data <- do.call(rbind, response_data$data)
  expect_setequal(colnames(data),
                  c("area_id", "area_name", "age_group", "sex",
                    "calendar_quarter", "indicator", "source", "mean",
                    "lower", "upper"))
  expect_true(nrow(data) > 0)
  expect_equal(names(response_data$plottingMetadata), "barchart")
  expect_setequal(names(response_data$plottingMetadata$barchart),
                  c("indicators", "filters", "defaults", "selections"))

  barchart_indicators <- do.call(
    rbind, response_data$plottingMetadata$barchart$indicators)
  expect_setequal(colnames(barchart_indicators),
                  c("indicator", "value_column", "error_low_column",
                    "error_high_column", "indicator_column", "indicator_value",
                    "indicator_sort_order", "name", "scale", "accuracy",
                    "format"))
  expect_true(nrow(barchart_indicators) > 0)

  filters <- lapply(response_data$plottingMetadata$barchart$filters, "[[",
                    "column_id")
  expect_equal(filters[[1]], "area_id")
  expect_equal(filters[[2]], "calendar_quarter")
  expect_equal(filters[[3]], "sex")
  expect_equal(filters[[4]], "age_group")
  expect_equal(filters[[5]], "source")

  expect_setequal(names(response_data$plottingMetadata$barchart$defaults),
                  c("indicator_id", "x_axis_id", "disaggregate_by_id",
                    "selected_filter_options"))

  expect_true(length(response_data$plottingMetadata$barchart$selections) >= 5)
  for (selection in response_data$plottingMetadata$barchart$selections) {
    expect_setequal(names(selection),
                    c("indicator_id", "x_axis_id", "disaggregate_by_id",
                      "selected_filter_options"))
  }
})
