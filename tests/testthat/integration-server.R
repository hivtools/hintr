withr::with_dir(testthat::test_path(), {
  server <- porcelain::porcelain_background$new(
    api, args = list(queue_id = paste0("hintr:", ids::random_id())))
  server$start()
})

test_that("Root", {
  r <- server$request("GET", "/")
  expect_equal(httr::status_code(r), 200)
  expect_equal(response_from_json(r)$data, "Welcome to hintr")
})

test_that("validate pjnz", {
  payload <- system_file("payload", "validate_pjnz_payload.json")
  r <- server$request(
    "POST", "/validate/baseline-individual",
    body = payload,
    httr::content_type_json())
  expect_equal(httr::status_code(r), 200)
  expect_equal(
    response_from_json(r),
    list(status = "success",
         errors = NULL,
         data = list(hash = "12345",
                     type = "pjnz",
                     data = list(country = "Malawi",
                                 iso3 = "MWI"),
                     filename = "Malawi2019.PJNZ",
                     fromADR = FALSE,
                     resource_url = "https://adr.unaids.org/file/123.csv",
                     filters = NULL)))
})

test_that("validate shape", {
  payload <- system_file("payload", "validate_shape_payload.json")
  r <- server$request(
    "POST", "/validate/baseline-individual",
    body = payload,
    httr::content_type_json())
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)

  expect_equal(response$status, "success")
  expect_equal(response$errors, NULL)
  expect_equal(response$data$hash, "12345")
  expect_equal(response$data$fromADR, FALSE)
  expect_equal(response$data$resource_url, NULL)
  expect_equal(response$data$filename, "original.geojson")
  expect_equal(response$data$type, "shape")
  expect_true(all(c("type", "features") %in% names(response$data$data)))
  expect_equal(response$data$data$type, "FeatureCollection")
  expect_equal(names(response$data$filters), c("regions", "level_labels"))
})

test_that("validate population", {
  payload <- system_file("payload", "validate_population_payload.json")
  r <- server$request(
    "POST", "/validate/baseline-individual",
    body = payload,
    httr::content_type_json())
  expect_equal(httr::status_code(r), 200)
  expect_equal(response_from_json(r), list(
    status = "success",
    errors = NULL,
    data = list(hash = "12345",
                type = "population",
                data = NULL,
                filename = "original.csv",
                fromADR = FALSE,
                resource_url = NULL,
                filters = NULL)))
})

test_that("validate programme", {
  payload <- system_file("payload", "validate_programme_payload.json")
  r <- server$request(
    "POST", "/validate/survey-and-programme",
    body = payload,
    httr::content_type_json())
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, NULL)
  expect_equal(response$data$hash, "12345")
  expect_equal(response$data$filename, "original.csv")
  expect_equal(response$data$type, "programme")
  expect_equal(response$data$fromADR, FALSE)
  expect_equal(response$data$resource_url, NULL)
  expect_true(length(response$data$data) >= 500)
  expect_type(response$data$data[[1]]$art_current, "integer")
  expect_equal(names(response$data$filters),
               c("age", "calendar_quarter", "indicators"))
  expect_length(response$data$filters$age, 2)
  expect_length(response$data$filters$calendar_quarter, 8)
  expect_length(response$data$filters$indicators, 4)
  expect_length(response$data$warnings, 0)
})

test_that("validate ANC", {
  payload <- system_file("payload", "validate_anc_payload.json")
  r <- server$request(
    "POST", "/validate/survey-and-programme",
    body = payload,
    httr::content_type_json())
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, NULL)
  expect_equal(response$data$hash, "12345")
  expect_equal(response$data$filename, "original.csv")
  expect_equal(response$data$type, "anc")
  expect_equal(response$data$fromADR, FALSE)
  expect_equal(response$data$resource_url,
               "https://adr.unaids.org/file/123.csv")
  expect_true(length(response$data$data) >= 200)
  expect_type(response$data$data[[1]]$anc_clients, "integer")
  expect_equal(names(response$data$filters), c("year", "indicators"))
  expect_length(response$data$filters$year, 8)
  expect_length(response$data$filters$indicators, 2)
  expect_length(response$data$warnings, 0)
})

test_that("validate survey", {
  payload <- system_file("payload", "validate_survey_payload.json")
  r <- server$request(
    "POST", "/validate/survey-and-programme",
    body = payload,
    httr::content_type_json())
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, NULL)
  expect_equal(response$data$hash, "12345")
  expect_equal(response$data$filename, "original.csv")
  expect_equal(response$data$type, "survey")
  expect_equal(response$data$fromADR, FALSE)
  expect_equal(response$data$resource_url, NULL)
  expect_true(length(response$data$data) >= 20000)
  expect_type(response$data$data[[1]]$estimate, "double")
  expect_equal(names(response$data$filters), c("age", "surveys", "indicators"))
  expect_length(response$data$filters$age, 23)
  expect_length(response$data$filters$surveys, 4)
  expect_length(response$data$filters$indicators, 4)
  expect_length(response$data$warnings, 0)
})

test_that("validate baseline", {
  payload <- system_file("payload", "validate_baseline_payload.json")
  r <- server$request(
    "POST", "/validate/baseline-combined",
    body = payload,
    httr::content_type_json())
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, NULL)
  expect_equal(response$data$consistent, TRUE)
})

test_that("model interactions", {
  test_mock_model_available()
  payload <- setup_payload_submit()

  ## Submit a model run
  r <- server$request(
    "POST", "/model/submit",
    body = payload,
    httr::content_type_json())
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, NULL)
  expect_equal(names(response$data), c("id"))

  ## Get the status
  testthat::try_again(4, {
    Sys.sleep(2)
    r <- server$request("GET", paste0("/model/status/", response$data$id))
    expect_equal(httr::status_code(r), 200)
    response <- response_from_json(r)
    expect_equal(response$status, "success")
    expect_equal(response$errors, NULL)
    expect_equal(response$data$done, TRUE)
    expect_equal(response$data$status, "COMPLETE")
    expect_equal(response$data$success, TRUE)
    expect_equal(response$data$queue, 0)
    expect_true("id" %in% names(response$data))
    expect_length(response$data$progress, 2)
    expect_equal(response$data$progress[[1]]$name, "Started mock model")
    expect_true(response$data$progress[[1]]$complete)
    expect_equal(response$data$progress[[2]]$name, "Finished mock model")
    expect_false(response$data$progress[[2]]$complete)
    expect_equal(response$data$progress[[2]]$helpText, "model running")
  })

  r <- server$request("GET", paste0("/model/debug/", response$data$id))
  expect_equal(httr::status_code(r), 200)
  expect_equal(httr::headers(r)$`content-type`, "application/octet-stream")
  expect_match(httr::headers(r)$`content-disposition`,
               'attachment; filename="[a-z0-9]+_\\d+-\\d+_naomi_debug.zip"')
  bin <- httr::content(r, "raw")
  zip <- tempfile(fileext = ".zip")
  writeBin(bin, zip)
  tmp <- tempfile()
  dir.create(tmp)
  zip::unzip(zip, exdir = tmp)
  expect_equal(dir(tmp), response$data$id)
  expect_setequal(dir(file.path(tmp, response$data$id)),
                  c("data.rds", "files"))
  dat <- readRDS(file.path(tmp, response$data$id, "data.rds"))
  expect_equal(dat$objects$data$pjnz$filename, "Malawi2019.PJNZ")

  ## Get the result
  r <- server$request("GET", paste0("/model/result/", response$data$id))
  expect_equal(httr::status_code(r), 200)
  result_response <- response_from_json(r)
  expect_equal(result_response$status, "success")
  expect_equal(result_response$errors, NULL)
  expect_equal(httr::status_code(r), 200)
  expect_equal(result_response$data$id, response$data$id)
  expect_true(result_response$data$complete)
})

test_that("real model can be run & calibrated by API", {
  payload <- setup_payload_submit()
  ## Results can be stored in specified results directory
  results_dir <- tempfile("results")
  dir.create(results_dir)

  queue_id <- paste0("hintr:", ids::random_id())
  test_server <- porcelain::porcelain_background$new(
    api,
    args = list(queue_id = queue_id,
                results_dir = results_dir),
    env = c("USE_MOCK_MODEL" = "false"))
  test_server$start()

  ## Workers started mock model off
  controller <- rrq::rrq_controller$new(queue_id = queue_id)
  res <- controller$message_send_and_wait("EVAL",
                                          "Sys.getenv('USE_MOCK_MODEL')")
  expect_equal(res, list("false", "false"), ignore_attr = TRUE)

  ## Submit a model run
  r <- test_server$request(
    "POST", "/model/submit",
    body = payload,
    httr::content_type_json())
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, NULL)
  expect_equal(names(response$data), c("id"))

  ## Get the status
  testthat::try_again(60, {
    Sys.sleep(5)
    r <- test_server$request("GET", paste0("/model/status/", response$data$id))
    expect_equal(httr::status_code(r), 200)
    response <- response_from_json(r)
    expect_equal(response$status, "success")
    expect_equal(response$errors, NULL)
    expect_equal(response$data$done, TRUE)
    expect_equal(response$data$status, "COMPLETE")
    expect_equal(response$data$success, TRUE)
    expect_equal(response$data$queue, 0)
    expect_true("id" %in% names(response$data))
    expect_length(response$data$progress, 4)
    expect_true(response$data$progress[[1]]$complete)
    expect_true(response$data$progress[[2]]$complete)
    expect_true(response$data$progress[[3]]$complete)
    expect_true(response$data$progress[[4]]$complete)
  })

  ## Get the result
  id <- response$data$id
  r <- test_server$request("GET", paste0("/model/result/", id))
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, NULL)
  expect_equal(httr::status_code(r), 200)
  expect_equal(response$data$id, id)
  expect_true(response$data$complete)

  ## Calibrate submit
  payload <- setup_payload_calibrate()
  r <- test_server$request(
    "POST",  paste0("/calibrate/submit/", id),
    body = payload,
    httr::content_type_json())

  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  calibrate_id <- response$data$id
  expect_true(!is.null(calibrate_id))

  ## Calibrate status
  testthat::try_again(10, {
    Sys.sleep(5)
    r <- test_server$request("GET", paste0("/calibrate/status/", calibrate_id))
    expect_equal(httr::status_code(r), 200)
    response <- response_from_json(r)
    expect_equal(response$data$id, calibrate_id)
    expect_true(response$data$done)
    expect_equal(response$data$status, "COMPLETE")
    expect_true(response$data$success)
    expect_equal(response$data$queue, 0)
    expect_match(response$data$progress[[1]],
                 "Saving outputs - [\\d.m\\s]+s elapsed", perl = TRUE)
  })

  ## Calibrate result
  r <- test_server$request("GET", paste0("/calibrate/result/", calibrate_id))
  ## Response has same structure content as model result endpoint
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, NULL)
  expect_equal(names(response$data), c("data", "plottingMetadata", "warnings"))
  expect_equal(names(response$data$data[[1]]),
               c("area_id", "sex", "age_group", "calendar_quarter",
                 "indicator", "mode", "mean", "lower", "upper"))
  expect_true(length(response$data$data) > 84042)
  expect_equal(names(response$data$plottingMetadata),
               c("barchart", "choropleth"))
})

test_that("plotting metadata is exposed", {
  r <- server$request("GET", paste0("/meta/plotting/", "MWI"))
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")

  expect_true(all(names(response$data) %in%
                    c("survey", "anc", "output", "programme")))
  expect_equal(names(response$data$survey), "choropleth")
  expect_equal(names(response$data$anc), "choropleth")
  expect_equal(names(response$data$output), c("barchart", "choropleth"))
  expect_equal(names(response$data$programme), "choropleth")
  expect_length(response$data$anc$choropleth$indicators, 2)
  expect_equal(response$data$anc$choropleth$indicators[[1]]$indicator,
               "anc_prevalence")
  expect_equal(response$data$anc$choropleth$indicators[[2]]$indicator,
               "anc_art_coverage")
  expect_equal(response$data$anc$choropleth$indicators[[1]]$name,
               "ANC HIV prevalence")
  expect_equal(response$data$anc$choropleth$indicators[[2]]$name,
               "ANC prior ART coverage")
})

test_that("default plotting metadata is exposed", {
  r <- server$request("GET", "/meta/plotting")
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")

  expect_true(all(names(response$data) %in%
                    c("survey", "anc", "output", "programme")))
  expect_equal(names(response$data$survey), "choropleth")
  expect_equal(names(response$data$anc), "choropleth")
  expect_equal(names(response$data$output), c("barchart", "choropleth"))
  expect_equal(names(response$data$programme), "choropleth")
  expect_length(response$data$anc$choropleth$indicators, 2)
  expect_equal(response$data$anc$choropleth$indicators[[1]]$indicator,
               "anc_prevalence")
  expect_equal(response$data$anc$choropleth$indicators[[2]]$indicator,
               "anc_art_coverage")
  expect_equal(response$data$anc$choropleth$indicators[[1]]$name,
               "ANC HIV prevalence")
  expect_equal(response$data$anc$choropleth$indicators[[2]]$name,
               "ANC prior ART coverage")
})

test_that("model run options are exposed", {
  payload <- system_file("payload", "model_run_options_payload.json")
  r <- server$request(
    "POST", "/model/options",
    body = payload,
    httr::content_type_json())
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, NULL)
  expect_equal(names(response$data), "controlSections")
  expect_length(response$data$controlSections, 6)

  general_section <- response$data$controlSections[[1]]
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

  survey_section <- response$data$controlSections[[2]]
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

  anc_section <- response$data$controlSections[[3]]
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

  art_section <- response$data$controlSections[[4]]
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

  expect_true(!is.null(response$version))
  expect_equal(names(response$version), c("hintr", "naomi", "rrq", "traduire"))
  expect_true(all(grepl("^(\\d+\\.)?(\\d+\\.)?(\\*|\\d+)$", response$version)))
})

test_that("model options can be validated", {
  payload <- system_file("payload", "validate_options_payload.json")

  r <- server$request(
    "POST", "/validate/options",
    body = payload,
    httr::content_type_json())

  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, NULL)
  expect_equal(names(response$data), c("valid", "warnings"))
  expect_equal(response$data$valid, TRUE)
})

test_that("version information is returned", {
  r <- server$request("GET", "/hintr/version")
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_setequal(names(response$data),
                  c("hintr", "naomi", "rrq", "traduire"))
})

test_that("Incorrect debug key gives reasonable error", {
  r <- server$request("GET", "/model/debug/abc")
  expect_equal(httr::status_code(r), 400)
  response <- response_from_json(r)
  expect_equal(response$status, "failure")
  expect_equal(response$errors[[1]]$error, "INVALID_TASK")
  expect_equal(response$errors[[1]]$detail, "Task 'abc' not found")
})

test_that("worker information is returned", {
  r <- server$request("GET", "/hintr/worker/status")
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_match(names(response$data), "^[a-z]+_[a-z]+_[12]$")
  expect_equal(response$data, list("IDLE", "IDLE"), ignore_attr = TRUE)
})

test_that("download streams bytes", {
  test_mock_model_available()
  payload <- setup_payload_submit()

  ## Run a model
  r <- server$request(
    "POST", "/model/submit",
    body = payload,
    httr::content_type_json())
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, NULL)
  expect_equal(names(response$data), c("id"))
  model_fit_id <- response$data$id

  ## Get the status
  testthat::try_again(5, {
    Sys.sleep(5)
    r <- server$request("GET", paste0("/model/status/", model_fit_id))
    expect_equal(httr::status_code(r), 200)
    response <- response_from_json(r)
    expect_equal(response$status, "success")
    expect_equal(response$data$status, "COMPLETE")
  })

  ## Calibrate submit
  payload <- setup_payload_calibrate()
  r <- server$request(
    "POST",  paste0("/calibrate/submit/", model_fit_id),
    body = payload,
    httr::content_type_json())

  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  calibrate_id <- response$data$id
  expect_true(!is.null(response$data$id))

  ## Calibrate status
  testthat::try_again(10, {
    Sys.sleep(5)
    r <- server$request("GET", paste0("/calibrate/status/", calibrate_id))
    expect_equal(httr::status_code(r), 200)
    response <- response_from_json(r)
    expect_equal(response$data$id, calibrate_id)
    expect_true(response$data$done)
    expect_equal(response$data$status, "COMPLETE")
    expect_true(response$data$success)
    expect_equal(response$data$queue, 0)
    expect_match(response$data$progress[[1]],
                 "Saving outputs - [\\d.m\\s]+s elapsed", perl = TRUE)
  })

  ## Start the download
  r <- server$request("POST",
                      paste0("/download/submit/spectrum/", calibrate_id))
  response <- response_from_json(r)
  expect_equal(httr::status_code(r), 200)
  expect_true(!is.null(response$data$id))
  expect_equal(response$status, "success")

  ## Get download status
  testthat::try_again(5, {
    Sys.sleep(5)
    status_res <- server$request("GET",
                                 paste0("/download/status/", response$data$id))
    expect_equal(httr::status_code(status_res), 200)
    status <- response_from_json(status_res)
    expect_equal(status$status, "success")
    expect_equal(status$data$done, TRUE)
    expect_equal(status$data$status, "COMPLETE")
    expect_equal(status$data$queue, 0)
    expect_length(status$data$progress, 1)
    expect_true(!is.null(status$data$id))
  })

  ## Get headers
  headers <- server$request("HEAD",
                            paste0("/download/result/", response$data$id),
                            httr::add_headers("Accept-Encoding" = ""))
  expect_equal(httr::status_code(headers), 200)
  expect_equal(httr::headers(headers)$`content-type`,
               "application/octet-stream")
  expect_match(httr::headers(headers)$`content-disposition`,
               'attachment; filename="MWI_naomi-output_\\d+-\\d+.zip"')

  size <- length(httr::content(headers))
  content_length <- as.numeric(httr::headers(headers)$`content-length`)
  ## It contains some content, won't be same length as precomputed
  ## model output as this is generated before calibration
  expect_true(content_length > 100000)

  ## Can stream bytes
  res <- server$request("GET", paste0("/download/result/", response$data$id),
                        httr::add_headers("Accept-Encoding" = ""))
  expect_equal(httr::headers(res)$`content-type`, "application/octet-stream")
  expect_match(httr::headers(res)$`content-disposition`,
               'attachment; filename="MWI_naomi-output_\\d+-\\d+.zip"')

  size <- length(httr::content(res))
  content_length <- as.numeric(httr::headers(res)$`content-length`)
  expect_equal(size, content_length)
  ## It contains some content, won't be same length as precomputed
  ## model output as this is generated before calibration
  expect_true(size > 100000)

  ## Can get ADR metadata
  adr_res <- server$request("GET", paste0("/meta/adr/", response$data$id))
  expect_equal(httr::status_code(r), 200)
  adr_r <- response_from_json(adr_res)
  expect_equal(names(adr_r$data), c("type", "description"))
  expect_equal(adr_r$data$type, "spectrum")
  expect_type(adr_r$data$description, "character")
})

test_that("can quit", {
  skip("Test is flakey")
  test_mock_model_available()

  expect_true(server$process$is_alive())

  server$process$read_error_lines()

  r <- tryCatch(
    httr::POST(paste0(server$url, "/hintr/stop")),
    error = identity)
  expect_type(r, "error")

  ## Sleep to give time for process to be killed before checking
  Sys.sleep(2)
  testthat::try_again(10, {
    expect_false(server$process$is_alive())
    Sys.sleep(1)
  })
})

test_that("404 pages have sensible schema", {
  r <- server$request("GET", "/meaning-of-life")
  expect_equal(r$status_code, 404)
  expect_equal(r$headers[["content-type"]], "application/json")

  dat <- httr::content(r, "parsed", encoding = "UTF-8")
  expect_equal(dat$status, "failure")
  expect_equal(dat$errors[[1]]$error,
               "NOT_FOUND")
  expect_equal(dat$errors[[1]]$detail,
               "GET /meaning-of-life is not a valid hintr path")
})

test_that("translation", {
  r <- server$request("GET", "/", httr::add_headers("Accept-Language" = "fr"))
  expect_equal(httr::status_code(r), 200)
  expect_equal(response_from_json(r)$data, "Bienvenue chez hintr")

  r <- server$request("GET", "/", httr::add_headers("Accept-Language" = "pt"))
  expect_equal(httr::status_code(r), 200)
  expect_equal(response_from_json(r)$data, "Bem-vindo ao hintr")
})

test_that("crashed worker can be detected", {
  ## Results can be stored in specified results directory
  results_dir <- tempfile("results")
  dir.create(results_dir)
  queue_id <- paste0("hintr:", ids::random_id())
  test_server <- porcelain::porcelain_background$new(
    api,
    args = list(queue_id = queue_id,
                results_dir = results_dir),
    env = c("USE_MOCK_MODEL" = "false"))
  test_server$start()

  ## Submit a model run
  payload <- setup_payload_submit()
  r <- test_server$request(
    "POST", "/model/submit",
    body = payload,
    httr::content_type_json())
  httr::stop_for_status(r)
  id <- response_from_json(r)$data$id

  Sys.sleep(2)
  obj <- rrq::rrq_controller$new(queue_id = queue_id)
  expect_equal(obj$task_status(id), setNames("RUNNING", id))

  ## There's quite a chore here to try and identify the actual running
  ## job. The worker process will (eventually) have 3 running
  ## subprocesses:
  ## - heartbeat process
  ## - processx supervisor
  ## - actual job
  ##
  ## We can use the ps package to get the tree of processes, and find
  ## the most recent one and kill that
  w <- obj$worker_task_id()
  expect_equal(unname(w), id)
  info <- obj$worker_info()[[names(w)]]
  children <- ps::ps_children(ps::ps_handle(info$pid))
  ps_task <- children[[which.max(vapply(children, ps::ps_pid, numeric(1)))]]
  ps::ps_send_signal(ps_task, ps::signals()$SIGTERM)

  Sys.sleep(2) # This really won't take long to come through

  r <- test_server$request("GET", paste0("/model/status/", id))

  expect_equal(httr::status_code(r), 200)
  dat <- response_from_json(r)
  expect_true(dat$data$done)
  expect_false(dat$data$success)
  expect_equal(dat$data$status, "DIED")

  r <- test_server$request("GET", paste0("/model/result/", id))
  expect_equal(httr::status_code(r), 400)
  dat <- response_from_json(r)
  expect_equal(dat$errors[[1]]$error,
               "MODEL_RUN_FAILED")
  expect_equal(dat$errors[[1]]$detail,
               "Worker has crashed - error details are unavailable")
  expect_type(dat$errors[[1]]$key, "character")
})

test_that("model run can be cancelled", {
  test_mock_model_available()
  payload <- setup_payload_submit()

  ## Submit a model run
  r <- server$request(
    "POST", "/model/submit",
    body = payload,
    httr::content_type_json())
  expect_equal(httr::status_code(r), 200)
  id <- response_from_json(r)$data$id

  r <- server$request("POST", paste0("/model/cancel/", id))
  expect_equal(httr::status_code(r), 200)
  dat <- response_from_json(r)
  expect_equal(dat$status, "success")
  expect_null(dat$data)

  testthat::try_again(5, {
    Sys.sleep(1)
    r <- server$request("GET", paste0("/model/status/", id))
    expect_equal(httr::status_code(r), 200)
    dat <- response_from_json(r)
    expect_equal(dat$status, "success")
    expect_true(dat$data$done)
    expect_equal(dat$data$status, "CANCELLED")
    expect_false(dat$data$success)
  })

  r <- server$request("GET", paste0("/model/result/", id))
  expect_equal(httr::status_code(r), 400)
  dat <- response_from_json(r)
  expect_equal(dat$status, "failure")
  expect_equal(dat$errors[[1]]$error,
               "MODEL_RUN_FAILED")
  expect_equal(dat$errors[[1]]$detail,
               "Model run was cancelled by user")
})

test_that("endpoint_model_submit can be run without anc or programme data", {
  test_mock_model_available()
  payload <- setup_payload_submit(include_anc_art = FALSE)

  ## Run a model
  r <- server$request(
    "POST", "/model/submit",
    body = payload,
    httr::content_type_json())
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, NULL)
  expect_equal(names(response$data), c("id"))
})

test_that("input time series can return plot data for programme", {
  programme_input <- setup_payload_input_time_series(
    test_path("testdata"),
    "programme.csv",
    "programme")
  r <- server$request(
    "POST", "/chart-data/input-time-series/programme",
    body = programme_input,
    encode = "json",
    httr::content_type_json())

  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, NULL)
  expect_equal(names(response$data), c("data", "metadata", "warnings"))
  expect_true(length(response$data$data) > 100)
  expect_equal(names(response$data$metadata$defaults$selected_filter_options),
               c("plot_type", "area_level", "quarter"))
})

test_that("input time series can return plot data for anc", {
  programme_input <- setup_payload_input_time_series(
    test_path("testdata"),
    "anc.csv",
    "anc")
  r <- server$request(
    "POST", "/chart-data/input-time-series/anc",
    body = programme_input,
    encode = "json",
    httr::content_type_json())

  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, NULL)
  expect_equal(names(response$data), c("data", "metadata", "warnings"))
  expect_true(length(response$data$data) > 100)
  expect_equal(names(response$data$metadata$defaults$selected_filter_options),
               c("plot_type", "area_level", "age", "quarter"))
})

test_that("rehydrate", {
  payload <- setup_payload_rehydrate()

  r <- server$request("POST",
                      "/rehydrate/submit",
                      body = payload,
                      encode = "json",
                      httr::content_type_json())
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, NULL)
  expect_equal(names(response$data), c("id"))
  id <- response$data$id

  ## Get the status
  testthat::try_again(10, {
    Sys.sleep(1)
    r <- server$request("GET", paste0("/rehydrate/status/", id))
    expect_equal(httr::status_code(r), 200)
    response <- response_from_json(r)
    expect_equal(response$status, "success")
    expect_equal(response$data$status, "COMPLETE")
  })

  ## Result
  r <- server$request("GET", paste0("/rehydrate/result/", id))
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, NULL)
  expect_setequal(names(response$data$state),
                  c("datasets", "model_fit", "calibrate", "version"))
  expect_setequal(
    names(response$data$state$datasets),
    c("pjnz", "population", "shape", "survey", "programme", "anc"))
  expect_match(response$data$notes, "These are my project notes")
})
