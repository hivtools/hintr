context("server")

test_that("Root", {
  server <- hintr_server()

  r <- httr::GET(server$url)
  expect_equal(httr::status_code(r), 200)
  expect_equal(response_from_json(r), "Welcome to hintr")
})

test_that("validate pjnz", {
  server <- hintr_server()

  payload <- file.path("payload", "validate_pjnz_payload.json")
  r <- httr::POST(paste0(server$url, "/validate/baseline-individual"),
                  body = httr::upload_file(payload),
                  encode = "json")
  expect_equal(httr::status_code(r), 200)
  expect_equal(
    response_from_json(r),
    list(status = "success",
         errors = list(),
         data = list(hash = "12345",
                     type = "pjnz",
                     data = list(country = "Botswana",
                                 iso3 = "BWA"),
                     filename = "original.PJNZ",
                     filters = NULL)))
})

test_that("validate shape", {
  server <- hintr_server()

  payload <- file.path("payload", "validate_shape_payload.json")
  r <- httr::POST(paste0(server$url, "/validate/baseline-individual"),
                  body = httr::upload_file(payload),
                  encode = "json")
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)

  expect_equal(response$status, "success")
  expect_equal(response$errors, list())
  expect_equal(response$data$hash, "12345")
  expect_equal(response$data$filename, "original.geojson")
  expect_equal(response$data$type, "shape")
  expect_true(all(c("type", "features") %in% names(response$data$data)))
  expect_equal(response$data$data$type, "FeatureCollection")
  expect_equal(names(response$data$filters), c("regions", "level_labels"))
})

test_that("validate population", {
  server <- hintr_server()

  payload <- file.path("payload", "validate_population_payload.json")
  r <- httr::POST(paste0(server$url, "/validate/baseline-individual"),
                  body = httr::upload_file(payload),
                  encode = "json")
  expect_equal(httr::status_code(r), 200)
  expect_equal(response_from_json(r),
               list(status = "success",
                    errors = list(),
                    data = list(hash = "12345",
                                type = "population",
                                data = NULL,
                                filename = "original.csv",
                                filters = NULL)))
})

test_that("validate programme", {
  server <- hintr_server()

  payload <- file.path("payload", "validate_programme_payload.json")
  r <- httr::POST(
    paste0(server$url, "/validate/survey-and-programme"),
    body = httr::upload_file(payload),
    encode = "json")
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, list())
  expect_equal(response$data$hash, "12345")
  expect_equal(response$data$filename, "original.csv")
  expect_equal(response$data$type, "programme")
  expect_true(length(response$data$data) >= 500)
  expect_equal(typeof(response$data$data[[1]]$current_art), "double")
  expect_equal(names(response$data$filters), c("age", "year", "indicators"))
  expect_length(response$data$filters$age, 2)
  expect_length(response$data$filters$year, 8)
  expect_length(response$data$filters$indicators, 1)
})

test_that("validate ANC", {
  server <- hintr_server()

  payload <- file.path("payload", "validate_anc_payload.json")
  r <- httr::POST(
    paste0(server$url, "/validate/survey-and-programme"),
    body = httr::upload_file(payload),
    encode = "json")
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, list())
  expect_equal(response$data$hash, "12345")
  expect_equal(response$data$filename, "original.csv")
  expect_equal(response$data$type, "anc")
  expect_true(length(response$data$data) >= 200)
  expect_equal(typeof(response$data$data[[1]]$ancrt_hiv_status), "integer")
  expect_equal(names(response$data$filters), c("year", "indicators"))
  expect_length(response$data$filters$year, 8)
  expect_length(response$data$filters$indicators, 2)
})

test_that("validate survey", {
  server <- hintr_server()

  payload <- file.path("payload", "validate_survey_payload.json")
  r <- httr::POST(
    paste0(server$url, "/validate/survey-and-programme"),
    body = httr::upload_file(payload),
    encode = "json")
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, list())
  expect_equal(response$data$hash, "12345")
  expect_equal(response$data$filename, "original.csv")
  expect_equal(response$data$type, "survey")
  expect_true(length(response$data$data) >= 20000)
  expect_equal(typeof(response$data$data[[1]]$est), "double")
  expect_equal(names(response$data$filters), c("age", "surveys", "indicators"))
  expect_length(response$data$filters$age, 21)
  expect_length(response$data$filters$surveys, 4)
  expect_length(response$data$filters$indicators, 4)
})

test_that("validate baseline", {
  server <- hintr_server()

  payload <- file.path("payload", "validate_baseline_payload.json")
  r <- httr::POST(paste0(server$url, "/validate/baseline-combined"),
                  body = httr::upload_file(payload),
                  encode = "json")
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, list())
  expect_equal(response$data$consistent, TRUE)
})

test_that("model interactions", {
  test_mock_model_available()
  payload <- setup_submit_payload()
  server <- hintr_server()

  ## Submit a model run
  r <- httr::POST(paste0(server$url, "/model/submit"),
                  body = httr::upload_file(payload),
                  encode = "json")
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, list())
  expect_equal(names(response$data), c("id"))

  ## Get the status
  testthat::try_again(4, {
    Sys.sleep(2)
    r <- httr::GET(paste0(server$url, "/model/status/", response$data$id))
    expect_equal(httr::status_code(r), 200)
    response <- response_from_json(r)
    expect_equal(response$status, "success")
    expect_equal(response$errors, list())
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
  })

  r <- httr::GET(paste0(server$url, "/model/debug/", response$data$id))
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

  path <- download_debug(response$data$id, server = server$url,
                         verbose = FALSE)
  expect_equal(dir(path, recursive = TRUE),
               dir(file.path(tmp, response$data$id), recursive = TRUE))

  ## Get the result
  r <- httr::GET(paste0(server$url, "/model/result/", response$data$id))
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, list())
  expect_equal(httr::status_code(r), 200)
  expect_equal(names(response$data), c("data", "plottingMetadata"))
  expect_equal(names(response$data$data[[1]]),
               c("area_id", "sex", "age_group", "calendar_quarter",
                 "indicator_id", "mode", "mean", "lower", "upper"))
  expect_true(length(response$data$data) > 84042)
  expect_equal(names(response$data$plottingMetadata),
               c("barchart", "choropleth"))

  barchart <- response$data$plottingMetadata$barchart
  expect_equal(names(barchart), c("indicators", "filters", "defaults"))
  expect_length(barchart$filters, 4)
  expect_equal(names(barchart$filters[[1]]),
               c("id", "column_id", "label", "options", "use_shape_regions"))
  expect_equal(names(barchart$filters[[2]]),
               c("id", "column_id", "label", "options"))
  expect_equal(barchart$filters[[1]]$id, "area")
  expect_equal(barchart$filters[[2]]$id, "quarter")
  expect_equal(barchart$filters[[3]]$id, "sex")
  expect_equal(barchart$filters[[4]]$id, "age")
  expect_true(length(barchart$filters[[4]]$options) >= 29)
  expect_length(barchart$filters[[2]]$options, 3)
  expect_equal(barchart$filters[[2]]$options[[2]]$id, "CY2018Q3")
  expect_equal(barchart$filters[[2]]$options[[2]]$label, "September 2018")
  expect_length(barchart$indicators, 10)
  out <- lapply(barchart$indicators, function(indicator) {
    expect_true(indicator$indicator %in%
                  c("prevalence", "art_coverage", "current_art", "population",
                    "plhiv", "incidence", "new_infections", "receiving_art",
                    "anc_prevalence", "anc_art_coverage"))
  })

  choropleth <- response$data$plottingMetadata$choropleth
  expect_equal(names(choropleth), c("indicators", "filters"))
  expect_length(choropleth$filters, 4)
  expect_equal(names(choropleth$filters[[1]]),
               c("id", "column_id", "label", "options", "use_shape_regions"))
  expect_equal(names(choropleth$filters[[2]]),
               c("id", "column_id", "label", "options"))
  expect_equal(choropleth$filters[[1]]$id, "area")
  expect_equal(choropleth$filters[[2]]$id, "quarter")
  expect_equal(choropleth$filters[[3]]$id, "sex")
  expect_equal(choropleth$filters[[4]]$id, "age")
  expect_true(length(choropleth$filters[[4]]$options) >= 29)
  expect_length(choropleth$filters[[2]]$options, 3)
  expect_equal(choropleth$filters[[2]]$options[[2]]$id, "CY2018Q3")
  expect_equal(choropleth$filters[[2]]$options[[2]]$label, "September 2018")
  expect_length(choropleth$indicators, 10)
  out <- lapply(choropleth$indicators, function(indicator) {
    expect_true(indicator$indicator %in%
                  c("prevalence", "art_coverage", "current_art", "population",
                    "plhiv", "incidence", "new_infections", "receiving_art",
                    "anc_prevalence", "anc_art_coverage"))
  })
})

test_that("real model can be run by API", {
  payload <- setup_submit_payload()
  ## Results can be stored in specified results directory
  results_dir <- tempfile("results")
  dir.create(results_dir)
  withr::with_envvar(c("USE_MOCK_MODEL" = "false"), {
    server <- hintr_server(results_dir = results_dir)

    ## Submit a model run
    r <- httr::POST(paste0(server$url, "/model/submit"),
                    body = httr::upload_file(payload),
                    encode = "json")
  })
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, list())
  expect_equal(names(response$data), c("id"))

  ## Get the status
  testthat::try_again(5, {
    Sys.sleep(60)
    r <- httr::GET(paste0(server$url, "/model/status/", response$data$id))
    expect_equal(httr::status_code(r), 200)
    response <- response_from_json(r)
    expect_equal(response$status, "success")
    expect_equal(response$errors, list())
    expect_equal(response$data$done, TRUE)
    expect_equal(response$data$status, "COMPLETE")
    expect_equal(response$data$success, TRUE)
    expect_equal(response$data$queue, 0)
    expect_true("id" %in% names(response$data))
    expect_length(response$data$progress, 5)
    expect_true(response$data$progress[[1]]$complete)
    expect_true(response$data$progress[[2]]$complete)
    expect_true(response$data$progress[[3]]$complete)
    expect_true(response$data$progress[[4]]$complete)
    expect_true(response$data$progress[[5]]$complete)
  })

  ## Get the result
  r <- httr::GET(paste0(server$url, "/model/result/", response$data$id))
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, list())
  expect_equal(httr::status_code(r), 200)
  expect_equal(names(response$data), c("data", "plottingMetadata"))
  expect_equal(names(response$data$data[[1]]),
               c("area_id", "sex", "age_group", "calendar_quarter",
                 "indicator_id", "mode", "mean", "lower", "upper"))
  expect_true(length(response$data$data) > 84042)
  expect_equal(names(response$data$plottingMetadata),
               c("barchart", "choropleth"))

  barchart <- response$data$plottingMetadata$barchart
  expect_equal(names(barchart), c("indicators", "filters", "defaults"))
  expect_length(barchart$filters, 4)
  expect_equal(names(barchart$filters[[1]]),
               c("id", "column_id", "label", "options", "use_shape_regions"))
  expect_equal(names(barchart$filters[[2]]),
               c("id", "column_id", "label", "options"))
  expect_equal(barchart$filters[[1]]$id, "area")
  expect_equal(barchart$filters[[2]]$id, "quarter")
  expect_equal(barchart$filters[[3]]$id, "sex")
  expect_equal(barchart$filters[[4]]$id, "age")
  expect_true(length(barchart$filters[[4]]$options) >= 29)
  expect_length(barchart$filters[[2]]$options, 3)
  expect_equal(barchart$filters[[2]]$options[[2]]$id, "CY2018Q3")
  expect_equal(barchart$filters[[2]]$options[[2]]$label, "September 2018")
  expect_length(barchart$indicators, 10)
  out <- lapply(barchart$indicators, function(indicator) {
    expect_true(indicator$indicator %in%
                  c("prevalence", "art_coverage", "current_art", "population",
                    "plhiv", "incidence", "new_infections", "receiving_art",
                    "anc_prevalence", "anc_art_coverage"))
  })

  choropleth <- response$data$plottingMetadata$choropleth
  expect_equal(names(choropleth), c("indicators", "filters"))
  expect_length(choropleth$filters, 4)
  expect_equal(names(choropleth$filters[[1]]),
               c("id", "column_id", "label", "options", "use_shape_regions"))
  expect_equal(names(choropleth$filters[[2]]),
               c("id", "column_id", "label", "options"))
  expect_equal(choropleth$filters[[1]]$id, "area")
  expect_equal(choropleth$filters[[2]]$id, "quarter")
  expect_equal(choropleth$filters[[3]]$id, "sex")
  expect_equal(choropleth$filters[[4]]$id, "age")
  expect_true(length(choropleth$filters[[4]]$options) >= 29)
  expect_length(choropleth$filters[[2]]$options, 3)
  expect_equal(choropleth$filters[[2]]$options[[2]]$id, "CY2018Q3")
  expect_equal(choropleth$filters[[2]]$options[[2]]$label, "September 2018")
  expect_length(choropleth$indicators, 10)
  out <- lapply(choropleth$indicators, function(indicator) {
    expect_true(indicator$indicator %in%
                  c("prevalence", "art_coverage", "current_art", "population",
                    "plhiv", "incidence", "new_infections", "receiving_art",
                    "anc_prevalence", "anc_art_coverage"))
  })
})

test_that("plotting metadata is exposed", {
  server <- hintr_server()
  r <- httr::GET(paste0(server$url, "/meta/plotting/", "MWI"))
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
               "prevalence")
  expect_equal(response$data$anc$choropleth$indicators[[2]]$indicator,
               "art_coverage")
  expect_equal(response$data$anc$choropleth$indicators[[1]]$name,
               "HIV prevalence")
  expect_equal(response$data$anc$choropleth$indicators[[2]]$name,
               "ART coverage")
})

test_that("model run options are exposed", {
  server <- hintr_server()
  options <- file.path("payload", "model_run_options_payload.json")
  r <- httr::POST(paste0(server$url, "/model/options"),
                  body = httr::upload_file(options),
                  encode = "json")
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, list())
  expect_equal(names(response$data), "controlSections")
  expect_length(response$data$controlSections, 7)

  general_section <- response$data$controlSections[[1]]
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
    "MWI2016PHIA")
  expect_equal(
    survey_section$controlGroups[[2]]$controls[[1]]$options[[1]]$label,
    "MWI2016PHIA")

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
  skip("Skipping model option validation endpoint not implemented mrc-592")
  server <- hintr_server()
  payload <- setup_submit_payload()

  r <- httr::POST(paste0(server$url, "/validate/options"),
                  body = httr::upload_file(payload),
                  encode = "json")

  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, list())
  expect_equal(names(response$data), "valid")
  expect_equal(response$data$valid, TRUE)
})

test_that("version information is returned", {
  server <- hintr_server()
  r <- httr::GET(paste0(server$url, "/hintr/version"))
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_setequal(names(response$data),
                  c("hintr", "naomi", "rrq", "traduire"))
})

test_that("Incorrect debug key gives reasonable error", {
  server <- hintr_server()
  r <- httr::GET(paste0(server$url, "/model/debug/abc"))
  expect_equal(httr::status_code(r), 400)
  response <- response_from_json(r)
  expect_equal(response$status, "failure")
  expect_equal(response$errors[[1]]$error, "INVALID_TASK")
  expect_equal(response$errors[[1]]$detail, "Task 'abc' not found")
})

test_that("worker information is returned", {
  server <- hintr_server()
  r <- httr::GET(paste0(server$url, "/hintr/worker/status"))
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_match(names(response$data), "^[a-z]+_[a-z]+_[12]$")
  expect_equivalent(response$data, list("IDLE", "IDLE"))
})

test_that("spectrum file download streams bytes", {
  test_mock_model_available()
  server <- hintr_server()
  payload <- setup_submit_payload()

  ## Run a model
  r <- httr::POST(paste0(server$url, "/model/submit"),
                  body = httr::upload_file(payload),
                  encode = "json")
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, list())
  expect_equal(names(response$data), c("id"))

  ## Get the download
  testthat::try_again(4, {
    Sys.sleep(2)
    r <- httr::GET(paste0(server$url, "/download/spectrum/", response$data$id))
    expect_equal(httr::status_code(r), 200)
    expect_equal(httr::headers(r)$`content-type`, "application/octet-stream")
    expect_match(httr::headers(r)$`content-disposition`,
                 'attachment; filename="MWI_\\d+-\\d+_naomi_spectrum_digest.zip"')
    ## Size of bytes is close to expected
    size <- as.numeric(httr::headers(r)$`content-length`)
    expect_true(size - size/10 <
      file.size(system_file("output", "malawi_spectrum_download.zip")))
    expect_true(size + size/10 >
      file.size(system_file("output", "malawi_spectrum_download.zip")))
  })

  ## Headers can be retrieved
  r <- httr::HEAD(paste0(server$url, "/download/spectrum/", response$data$id))
  expect_equal(httr::status_code(r), 200)
  expect_equal(httr::headers(r)$`content-type`, "application/octet-stream")
  expect_match(httr::headers(r)$`content-disposition`,
    'attachment; filename="MWI_\\d+-\\d+_naomi_spectrum_digest.zip"')
})

test_that("summary file download streams bytes", {
  test_mock_model_available()
  server <- hintr_server()
  payload <- setup_submit_payload()

  ## Run a model
  r <- httr::POST(paste0(server$url, "/model/submit"),
                  body = httr::upload_file(payload),
                  encode = "json")
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, list())
  expect_equal(names(response$data), c("id"))

  ## Get the download
  testthat::try_again(4, {
    Sys.sleep(2)
    r <- httr::GET(paste0(server$url, "/download/summary/", response$data$id))
    expect_equal(httr::status_code(r), 200)
    expect_equal(httr::headers(r)$`content-type`, "application/octet-stream")
    expect_match(
      httr::headers(r)$`content-disposition`,
      'attachment; filename="MWI_\\d+-\\d+_naomi_coarse_age_groups.zip"')
    ## Size of bytes is close to expected
    size <- as.numeric(httr::headers(r)$`content-length`)
    expect_true(size - size/10 <
      file.size(system_file("output", "malawi_summary_download.zip")))
    expect_true(size + size/10 >
      file.size(system_file("output", "malawi_summary_download.zip")))
  })

  ## Headers can be retrieved
  r <- httr::HEAD(paste0(server$url, "/download/summary/", response$data$id))
  expect_equal(httr::status_code(r), 200)
  expect_equal(httr::headers(r)$`content-type`, "application/octet-stream")
  expect_match(
    httr::headers(r)$`content-disposition`,
    'attachment; filename="MWI_\\d+-\\d+_naomi_coarse_age_groups.zip"')
})

test_that("can quit", {
  test_mock_model_available()
  server <- hintr_server()

  expect_true(server$process$is_alive())

  server$process$read_error_lines()

  r <- tryCatch(
    httr::POST(paste0(server$url, "/hintr/stop")),
    error = identity)
  expect_is(r, "error")

  testthat::try_again(10, {
    expect_false(server$process$is_alive())
    Sys.sleep(1)
  })
})

test_that("404 pages have sensible schema", {
  server <- hintr_server()
  r <- httr::GET(paste0(server$url, "/meaning-of-life"))
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
  server <- hintr_server()

  r <- httr::GET(server$url, httr::add_headers("Accept-Language" = "fr"))
  expect_equal(httr::status_code(r), 200)
  expect_equal(response_from_json(r), "Bienvenue chez hintr")
})

test_that("crashed worker can be detected", {
  ## Results can be stored in specified results directory
  results_dir <- tempfile("results")
  dir.create(results_dir)
  withr::with_envvar(c("USE_MOCK_MODEL" = "false"), {
    server <- hintr_server(results_dir = results_dir)
  })

  ## Submit a model run
  payload <- setup_submit_payload()
  r <- httr::POST(paste0(server$url, "/model/submit"),
                  body = httr::upload_file(payload),
                  encode = "json")
  httr::stop_for_status(r)
  id <- response_from_json(r)$data$id

  Sys.sleep(2)
  obj <- rrq::rrq_controller(server$queue_id)
  expect_equal(obj$task_status(id), setNames("RUNNING", id))

  w <- obj$worker_task_id()
  expect_equal(unname(w), id)
  info <- obj$worker_info()[[names(w)]]
  tools::pskill(info$pid)

  Sys.sleep(10) # 3 * the testing heartbeat + 1

  r <- httr::GET(paste0(server$url, "/model/status/", id))
  expect_equal(httr::status_code(r), 200)
  dat <- response_from_json(r)
  expect_true(dat$data$done)
  expect_false(dat$data$success)
  expect_equal(dat$data$status, "ORPHAN")

  r <- httr::GET(paste0(server$url, "/model/result/", id))
  expect_equal(httr::status_code(r), 400)
  dat <- response_from_json(r)
  expect_equal(dat$errors[[1]]$error,
               "MODEL_RUN_FAILED")
  expect_equal(dat$errors[[1]]$detail,
               "Worker has crashed - error details are unavailable")
  expect_is(dat$errors[[1]]$key, "character")
})

test_that("model run can be cancelled", {
  test_mock_model_available()
  payload <- setup_submit_payload()
  server <- hintr_server()

  ## Submit a model run
  r <- httr::POST(paste0(server$url, "/model/submit"),
                  body = httr::upload_file(payload),
                  encode = "json")
  expect_equal(httr::status_code(r), 200)
  id <- response_from_json(r)$data$id

  r <- httr::POST(paste0(server$url, "/model/cancel/", id))
  expect_equal(httr::status_code(r), 200)
  dat <- response_from_json(r)
  expect_equal(dat$status, "success")
  expect_null(dat$data)

  r <- httr::GET(paste0(server$url, "/model/status/", id))
  expect_equal(httr::status_code(r), 200)
  dat <- response_from_json(r)
  expect_equal(dat$status, "success")
  expect_true(dat$data$done)
  expect_equal(dat$data$status, "INTERRUPTED")
  expect_false(dat$data$success)

  r <- httr::GET(paste0(server$url, "/model/result/", id))
  expect_equal(httr::status_code(r), 400)
  dat <- response_from_json(r)
  expect_equal(dat$status, "failure")
  expect_equal(dat$errors[[1]]$error,
               "MODEL_RUN_FAILED")
  expect_equal(dat$errors[[1]]$detail,
               "Model run was cancelled by user")
})

test_that("download_debug prevents overwriting", {
  tmp <- tempfile()
  id <- "abc"
  dir.create(file.path(tmp, id), FALSE, TRUE)
  expect_error(
    download_debug(id, dest = tmp),
    "Path 'abc' already exists at destination")
})
