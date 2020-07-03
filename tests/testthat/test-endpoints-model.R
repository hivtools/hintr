context("endpoints-model")

test_that("endpoint model run queues a model run", {
  test_redis_available()
  test_mock_model_available()
  ## Create request data
  data <- list(
    pjnz = list(path = "path/to/pjnz", hash = "12345", filename = "original"),
    shape = list(path = "path/to/shape", hash = "12345", filename = "original"),
    population = list(path = "path/to/pop", hash = "12345", filename = "original"),
    survey = list(path = "path/to/survey", hash = "12345", filename = "original"),
    programme = list(path = "path/to/programme", hash = "12345", filename = "original"),
    anc = list(path = "path/to/anc", hash = "12345", filename = "original")
  )
  options = list()
  req <- list(postBody = '
              {
              "data": {
              "pjnz": {"path":"path/to/file","hash": "12345","filename":"original"}
              "shape":  {"path":"path/to/file","hash": "12345","filename":"original"},
              "population":  {"path":"path/to/file","hash": "12345","filename":"original"},
              "survey":  {"path":"path/to/file","hash": "12345","filename":"original"},
              "programme":  {"path":"path/to/file","hash": "12345","filename":"original"},
              "anc":  {"path":"path/to/file","hash": "12345","filename":"original"}
              },
              "options": {}
              }')

  ## Create mock response
  res <- MockPlumberResponse$new()

  ## Call the endpoint
  queue <- test_queue()
  model_submit <- endpoint_model_submit(queue)
  response <- model_submit(req, res, data, options, cfg$version_info)
  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "success")
  expect_true("id" %in% names(response$data))
  expect_equal(res$status, 200)

  ## Wait for complete and query for status
  ## Query for status
  testthat::try_again(5, {
    result <- queue$queue$task_wait(response$data$id)
    res <- MockPlumberResponse$new()
    model_status <- endpoint_model_status(queue)
    status <- model_status(NULL, res, response$data$id)
    status <- jsonlite::parse_json(status)
    expect_equal(res$status, 200)
    expect_equal(status$status, "success")
    expect_equal(status$data$id, response$data$id)
    expect_equal(status$data$done, TRUE)
    expect_equal(status$data$status, "COMPLETE")
    expect_equal(status$data$queue, 0)
    expect_equal(status$data$success, TRUE)
    expect_length(status$data$progress, 2)
    expect_equal(status$data$progress[[1]]$name, "Started mock model")
    expect_true(status$data$progress[[1]]$complete)
    expect_equal(status$data$progress[[2]]$name, "Finished mock model")
    expect_false(status$data$progress[[2]]$complete)
    Sys.sleep(2)
  })

  ## Get the result
  res <- MockPlumberResponse$new()
  model_result <- endpoint_model_result(queue)
  result <- model_result(NULL, res, status$data$id)
  result <- jsonlite::parse_json(result)
  expect_equal(res$status, 200)
  expect_equal(names(result$data), c("data", "plottingMetadata"))
  expect_equal(names(result$data$data[[1]]),
               c("area_id", "sex", "age_group", "calendar_quarter",
                 "indicator_id", "mode", "mean", "lower", "upper"))
  expect_true(length(result$data$data) > 84042)
  expect_equal(names(result$data$plottingMetadata), c("barchart", "choropleth"))


  ## Barchart
  barchart <- result$data$plottingMetadata$barchart
  expect_equal(names(barchart), c("indicators", "filters", "defaults"))
  expect_length(barchart$filters, 4)
  expect_equal(names(barchart$filters[[1]]),
               c("id", "column_id", "label", "options", "use_shape_regions"))
  expect_equal(names(barchart$filters[[2]]),
               c("id", "column_id", "label", "options"))
  ## Choropleth has the correct filters in correct order
  filters <- lapply(barchart$filters, function(filter) {
    filter$column_id
  })
  expect_equal(filters[[1]], "area_id")
  expect_equal(filters[[2]], "calendar_quarter")
  expect_equal(filters[[3]], "sex")
  expect_equal(filters[[4]], "age_group")
  expect_length(barchart$filters[[2]]$options, 3)
  expect_equal(barchart$filters[[2]]$options[[2]]$id, "CY2018Q3")
  expect_equal(barchart$filters[[2]]$options[[2]]$label, "September 2018")
  expect_true(length(barchart$filters[[4]]$options) >= 29)
  expect_length(barchart$indicators, 10)

  ## Quarters are in descending order
  calendar_quarters <-
    lapply(barchart$filters[[2]]$options, function(option) {
      option$id
    })
  expect_equal(unlist(calendar_quarters),
               sort(unlist(calendar_quarters), decreasing = TRUE))


  ## Barchart indicators are in numeric id order
  indicators <- lapply(barchart$indicators, function(indicator) {
    indicator$indicator
  })
  expect_equal(unlist(indicators),
               c("population", "prevalence", "plhiv", "art_coverage",
                 "current_art", "receiving_art", "incidence", "new_infections",
                 "anc_prevalence", "anc_art_coverage"))

  ## Choropleth
  choropleth <- result$data$plottingMetadata$choropleth
  expect_equal(names(choropleth), c("indicators", "filters"))
  expect_length(choropleth$filters, 4)
  expect_equal(names(choropleth$filters[[1]]),
               c("id", "column_id", "label", "options", "use_shape_regions"))
  expect_equal(names(choropleth$filters[[2]]),
               c("id", "column_id", "label", "options"))
  ## Choropleth has the correct filters in correct order
  filters <- lapply(choropleth$filters, function(filter) {
    filter$column_id
  })
  expect_equal(filters[[1]], "area_id")
  expect_equal(filters[[2]], "calendar_quarter")
  expect_equal(filters[[3]], "sex")
  expect_equal(filters[[4]], "age_group")
  expect_length(choropleth$filters[[2]]$options, 3)
  expect_equal(choropleth$filters[[2]]$options[[2]]$id, "CY2018Q3")
  expect_equal(choropleth$filters[[2]]$options[[2]]$label, "September 2018")
  expect_true(length(choropleth$filters[[4]]$options) >= 29)
  expect_length(choropleth$indicators, 10)

  ## Quarters are in descending order
  calendar_quarters <-
    lapply(choropleth$filters[[2]]$options, function(option) {
      option$id
    })
  expect_equal(unlist(calendar_quarters),
               sort(unlist(calendar_quarters), decreasing = TRUE))

  ## Choropleth indicators are in numeric id order
  indicators <- lapply(choropleth$indicators, function(indicator) {
    indicator$indicator
  })
  expect_equal(unlist(indicators),
               c("population", "prevalence", "plhiv", "art_coverage",
                 "current_art", "receiving_art", "incidence", "new_infections",
                 "anc_prevalence", "anc_art_coverage"))
})

test_that("endpoint_run_model returns error if queueing fails", {
  test_redis_available()
  ## Create request data
  data <- list(
    pjnz = list(path = "path/to/pjnz", hash = "12345", filename = "original"),
    shape = list(path = "path/to/shape", hash = "12345", filename = "original"),
    population = list(path = "path/to/pop", hash = "12345", filename = "original"),
    survey = list(path = "path/to/survey", hash = "12345", filename = "original"),
    programme = list(path = "path/to/programme", hash = "12345", filename = "original"),
    anc = list(path = "path/to/anc", hash = "12345", filename = "original")
  )
  options = list(
    programme = TRUE,
    anc = FALSE
  )
  req <- list(postBody = '
              {
              "data": {
              "pjnz": {"path":"path/to/file","hash": "12345","filename":"original"}
              "shape":  {"path":"path/to/file","hash": "12345","filename":"original"},
              "population":  {"path":"path/to/file","hash": "12345","filename":"original"},
              "survey":  {"path":"path/to/file","hash": "12345","filename":"original"},
              "programme":  {"path":"path/to/file","hash": "12345","filename":"original"},
              "anc":  {"path":"path/to/file","hash": "12345","filename":"original"}
              }
              }')

  ## Create mocks
  res <- MockPlumberResponse$new()
  queue <- test_queue()
  mock_submit <- function(data, options) { stop("Failed to queue") }

  ## Call the endpoint
  model_submit <- endpoint_model_submit(queue)
  mockery::stub(model_submit, "queue$submit", mock_submit)
  response <- model_submit(req, res, data, options, cfg$version_info)
  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "failure")
  expect_length(response$errors, 1)
  expect_equal(response$errors[[1]]$error, "FAILED_TO_QUEUE")
  expect_equal(response$errors[[1]]$detail, "Failed to queue")
  expect_equal(res$status, 400)
})

test_that("querying for status of missing job returns useful message", {
  test_redis_available()

  res <- MockPlumberResponse$new()
  queue <- test_queue()
  model_status <- endpoint_model_status(queue)
  status <- model_status(NULL, res, "ID")
  status <- jsonlite::parse_json(status)
  expect_equal(res$status, 200)
  expect_equal(status$status, "success")
  expect_null(status$data$done)
  expect_equal(status$data$status, "MISSING")
  expect_null(status$data$success)
  expect_equal(status$data$id, "ID")
})

test_that("querying for result of missing job returns useful error", {
  test_redis_available()

  res <- MockPlumberResponse$new()
  queue <- test_queue()
  model_result <- endpoint_model_result(queue)
  result <- model_result(NULL, res, "ID")
  result <- jsonlite::parse_json(result)
  expect_equal(res$status, 400)
  expect_equal(result$status, "failure")
  expect_length(result$data, 0)
  expect_length(result$errors, 1)
  expect_equal(result$errors[[1]]$error, "FAILED_TO_RETRIEVE_RESULT")
  expect_equal(result$errors[[1]]$detail, "Failed to fetch result")
})

test_that("querying for an orphan task returns sensible error", {
  test_redis_available()
  res <- MockPlumberResponse$new()
  queue <- test_queue(workers = 0)
  model_result <- endpoint_model_result(queue)

  id <- ids::random_id()
  queue$queue$con$HSET(queue$queue$keys$task_status, id, "ORPHAN")

  result <- jsonlite::parse_json(model_result(NULL, res, id))
  expect_equal(res$status, 400)
  expect_equal(result$status, "failure")
  expect_length(result$data, 0)
  expect_length(result$errors, 1)
  expect_equal(result$errors[[1]]$error, "MODEL_RUN_FAILED")
  expect_equal(result$errors[[1]]$detail,
               "Worker has crashed - error details are unavailable")
})

test_that("endpoint_run_status returns error if query for status fails", {
  test_redis_available()

  ## Create mocks
  res <- MockPlumberResponse$new()
  queue <- test_queue()
  mock_status <- function(data, parameters) { stop("Failed to get status") }

  ## Call the endpoint
  model_status <- endpoint_model_status(queue)
  mockery::stub(model_status, "queue$status", mock_status)
  response <- model_status(req, res, "ID")
  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "failure")
  expect_length(response$errors, 1)
  expect_equal(response$errors[[1]]$error, "FAILED_TO_RETRIEVE_STATUS")
  expect_equal(response$errors[[1]]$detail, "Failed to get status")
  expect_equal(res$status, 400)
})

test_that("querying for result of incomplete jobs returns useful error", {
  test_redis_available()
  data <- list(
    pjnz = list(path = "path/to/pjnz", hash = "12345", filename = "original"),
    shape = list(path = "path/to/shape", hash = "12345",  filename = "original"),
    population = list(path = "path/to/pop", hash = "12345", filename = "original"),
    survey = list(path = "path/to/survey", hash = "12345", filename = "original"),
    programme = list(path = "path/to/programme", hash = "12345", filename = "original"),
    anc = list(path = "path/to/anc", hash = "12345", filename = "original")
  )
  options = list()
  req <- list(postBody = '
              {
              "data": {
              "pjnz": {"path":"path/to/file","hash": "12345","filename":"original"}
              "shape":  {"path":"path/to/file","hash": "12345","filename":"original"},
              "population":  {"path":"path/to/file","hash": "12345","filename":"original"},
              "survey":  {"path":"path/to/file","hash": "12345","filename":"original"},
              "programme":  {"path":"path/to/file","hash": "12345","filename":"original"},
              "anc":  {"path":"path/to/file","hash": "12345","filename":"original"}
              },
              "options": {
              "use_mock_model": true
              }
              }')

  ## Create mock response
  res <- MockPlumberResponse$new()

  ## Call the endpoint
  queue <- test_queue()
  model_submit <- endpoint_model_submit(queue)
  response <- model_submit(req, res, data, options, cfg$version_info)
  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "success")

  ## Get result prematurely
  model_result <- endpoint_model_result(queue)
  result <- model_result(NULL, res, response$data$id)
  result <- jsonlite::parse_json(result)
  expect_equal(res$status, 400)
  expect_equal(result$status, "failure")
  expect_length(result$data, 0)
  expect_length(result$errors, 1)
  expect_equal(result$errors[[1]]$error, "FAILED_TO_RETRIEVE_RESULT")
  expect_equal(result$errors[[1]]$detail, "Failed to fetch result")
})

test_that("erroring model run returns useful messages", {
  test_redis_available()

  res <- MockPlumberResponse$new()

  ## Call the endpoint
  queue <- MockQueue$new()
  model_submit <- endpoint_model_submit(queue)
  response <- model_submit(req, res, NULL, list(), cfg$version_info)
  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "success")

  ## Get the status
  model_status <- endpoint_model_status(queue)
  status <- model_status(req, res, response$data$id)
  status <- jsonlite::parse_json(status)
  expect_equal(status$status, "success")
  expect_length(status$errors, 0)
  expect_equal(status$data$done, TRUE)
  expect_equal(status$data$status, "ERROR")
  expect_equal(status$data$success, FALSE)
  expect_equal(status$data$id, response$data$id)

  ## Get the result
  model_result <- endpoint_model_result(queue)
  result <- model_result(req, res, response$data$id)
  result_parsed <- jsonlite::parse_json(result)
  expect_equal(res$status, 400)

  expect_equal(result_parsed$status, "failure")
  expect_length(result_parsed$data, 0)
  expect_length(result_parsed$errors, 1)
  expect_equal(result_parsed$errors[[1]]$error, "MODEL_RUN_FAILED")
  expect_equal(result_parsed$errors[[1]]$detail, "test error")

  trace <- vcapply(result_parsed$errors[[1]]$trace, identity)
  expect_true("rrq:::rrq_worker_main()" %in% trace)
  expect_true("stop(\"test error\")" %in% trace)
  expect_match(trace[[1]], "^# [[:xdigit:]]+$")

  ## Check logging:
  res$headers[["Content-Type"]] <- "application/json"
  res$body <- result
  res$status <- 400
  msg <- capture_messages(
    api_log_end(NULL, NULL, res, NULL))
  expect_match(msg[[1]], "error-key: [a-z]{5}-[a-z]{5}-[a-z]{5}")
  expect_match(msg[[2]], "error-detail: test error")
  expect_match(msg[[3]], "error-trace: rrq:::rrq_worker_main")
})

test_that("running model with old version throws an error", {
  test_redis_available()

  ## Setup mocks
  res <- MockPlumberResponse$new()

  ## Call the endpoint
  queue <- test_queue()
  model_submit <- endpoint_model_submit(queue)
  version <- list(
    hintr = "0.0.12",
    naomi = "0.0.15",
    rrq = "0.2.1"
  )
  response <- model_submit(req, res, NULL, list(), version)
  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "failure")
  expect_length(response$errors, 1)
  expect_equal(response$errors[[1]]$error, "VERSION_OUT_OF_DATE")
  expect_equal(response$errors[[1]]$detail,
               "Trying to run model with old version of options. Update model run options")
  expect_length(response$data, 0)
})

test_that("model run can be cancelled", {
  test_redis_available()
  test_mock_model_available()
  ## Create request data
  data <- list(
    pjnz = list(path = "path/to/pjnz", hash = "12345", filename = "original"),
    shape = list(path = "path/to/shape", hash = "12345", filename = "original"),
    population = list(path = "path/to/pop", hash = "12345", filename = "original"),
    survey = list(path = "path/to/survey", hash = "12345", filename = "original"),
    programme = list(path = "path/to/programme", hash = "12345", filename = "original"),
    anc = list(path = "path/to/anc", hash = "12345", filename = "original")
  )
  options = list()
  req <- list(postBody = '
              {
              "data": {
              "pjnz": {"path":"path/to/file","hash": "12345","filename":"original"}
              "shape":  {"path":"path/to/file","hash": "12345","filename":"original"},
              "population":  {"path":"path/to/file","hash": "12345","filename":"original"},
              "survey":  {"path":"path/to/file","hash": "12345","filename":"original"},
              "programme":  {"path":"path/to/file","hash": "12345","filename":"original"},
              "anc":  {"path":"path/to/file","hash": "12345","filename":"original"}
              },
              "options": {}
              }')

  queue <- test_queue()

  model_submit <- endpoint_model_submit(queue)
  model_cancel <- endpoint_model_cancel(queue)
  model_status <- endpoint_model_status(queue)
  model_result <- endpoint_model_result(queue)

  res <- MockPlumberResponse$new()
  response <- model_submit(req, res, data, options, cfg$version_info)
  id <- jsonlite::parse_json(response)$data$id

  running <- queue$queue$worker_task_id()
  expect_equal(unname(running), id)
  worker <- names(running)
  expect_equal(queue$queue$task_status(id), setNames("RUNNING", id))

  res <- MockPlumberResponse$new()
  response <- model_cancel(list(), res, id)
  expect_equal(jsonlite::parse_json(response),
               list(status = "success", errors = list(), data = NULL))

  testthat::try_again(5, {
    Sys.sleep(1)
    log <- queue$queue$worker_log_tail(worker, n = Inf)
    expect_true("INTERRUPT" %in% log$command)
    expect_equal(queue$queue$task_status(id), setNames("INTERRUPTED", id))
  })

  res <- MockPlumberResponse$new()
  response <- jsonlite::parse_json(model_status(list(), res, id))
  expect_equal(res$status, 200)
  expect_equal(response$status, "success")
  expect_true(response$data$done)
  expect_false(response$data$success)
  expect_equal(response$data$status, "INTERRUPTED")

  res <- MockPlumberResponse$new()
  response <- jsonlite::parse_json(model_result(list(), res, id))
  expect_equal(res$status, 400)
  expect_equal(response$status, "failure")
  expect_equal(response$errors[[1]]$error, "MODEL_RUN_FAILED")
  expect_equal(response$errors[[1]]$detail,
               "Model run was cancelled by user")
})

test_that("translation of progress", {
  test_redis_available()
  test_mock_model_available()
  ## Create request data
  data <- list(
    pjnz = list(path = "path/to/pjnz", hash = "12345", filename = "original"),
    shape = list(path = "path/to/shape", hash = "12345", filename = "original"),
    population = list(path = "path/to/pop", hash = "12345", filename = "original"),
    survey = list(path = "path/to/survey", hash = "12345", filename = "original"),
    programme = list(path = "path/to/programme", hash = "12345", filename = "original"),
    anc = list(path = "path/to/anc", hash = "12345", filename = "original")
  )
  options = list()
  req <- list(postBody = '
              {
              "data": {
              "pjnz": {"path":"path/to/file","hash": "12345","filename":"original"}
              "shape":  {"path":"path/to/file","hash": "12345","filename":"original"},
              "population":  {"path":"path/to/file","hash": "12345","filename":"original"},
              "survey":  {"path":"path/to/file","hash": "12345","filename":"original"},
              "programme":  {"path":"path/to/file","hash": "12345","filename":"original"},
              "anc":  {"path":"path/to/file","hash": "12345","filename":"original"}
              },
              "options": {}
              }')

  ## Create mock response
  res <- MockPlumberResponse$new()

  ## Call the endpoint
  queue <- test_queue()
  model_submit <- endpoint_model_submit(queue)
  model_status <- endpoint_model_status(queue)

  response <- with_hintr_language(
    "fr",
    model_submit(req, res, data, options, cfg$version_info))

  response <- jsonlite::parse_json(response)
  id <- response$data$id

  ## Query for status
  testthat::try_again(5, {
    result <- queue$queue$task_wait(id)
    res <- MockPlumberResponse$new()
    status <- model_status(NULL, res, id)
  })

  value <- jsonlite::parse_json(status)
  expect_equal(value$data$progress[[1]]$name,
               "Maquette commencée")
  expect_equal(value$data$progress[[2]]$name,
               "Maquette terminée")
})

test_that("error messages from naomi are translated", {
  test_redis_available()
  data <- list(
    pjnz = file.path("testdata", "Malawi2019.PJNZ"),
    shape = file.path("testdata", "malawi.geojson"),
    population = file.path("testdata", "population.csv"),
    survey = file.path("testdata", "survey.csv"),
    programme = file.path("testdata", "programme.csv"),
    anc = file.path("testdata", "anc.csv")
  )
  data <- list(
    pjnz = list(path = file.path("testdata", "Malawi2019.PJNZ"), hash = "12345", filename = "original"),
    shape = list(path = file.path("testdata", "malawi.geojson"), hash = "12345", filename = "original"),
    population = list(path = file.path("testdata", "population.csv"), hash = "12345", filename = "original"),
    survey = list(path = file.path("testdata", "survey.csv"), hash = "12345", filename = "original"),
    programme = list(path = file.path("testdata", "programme.csv"), hash = "12345", filename = "original"),
    anc = list(path = file.path("testdata", "anc.csv"), hash = "12345", filename = "original")
  )
  options <- list(
    area_scope = "MWI",
    area_level = 0,
    calendar_quarter_t1 = "CY2016Q1",
    calendar_quarter_t2 = "CY2018Q3",
    calendar_quarter_t3 = "CY2019Q2",
    survey_prevalence = c("MWI2016PHIA", "MWI2015DHS"),
    survey_art_coverage = "MWI2016PHIA",
    survey_recently_infected = "MWI2016PHIA",
    include_art_t1 = "true",
    include_art_t2 = "true",
    anc_prevalence_year1 = 2016,
    anc_prevalence_year2 = 2018,
    anc_art_coverage_year1 = 2016,
    anc_art_coverage_year2 = 2018,
    spectrum_population_calibration = "none",
    spectrum_plhiv_calibration_level = "none",
    spectrum_plhiv_calibration_strat = "sex_age_group",
    spectrum_artnum_calibration_level = "none",
    spectrum_artnum_calibration_strat = "age_coarse",
    spectrum_infections_calibration_level = "none",
    spectrum_infections_calibration_strat = "age_coarse",
    artattend_log_gamma_offset = -4L,
    artattend = "false",
    rng_seed = 17,
    no_of_samples = 20,
    max_iter = 250,
    permissive = "false"
  )
  req <- list(postBody = '
              {
              "data": {
              "pjnz": {"path":"path/to/file","hash": "12345","filename":"original"}
              "shape":  {"path":"path/to/file","hash": "12345","filename":"original"},
              "population":  {"path":"path/to/file","hash": "12345","filename":"original"},
              "survey":  {"path":"path/to/file","hash": "12345","filename":"original"},
              "programme":  {"path":"path/to/file","hash": "12345","filename":"original"},
              "anc":  {"path":"path/to/file","hash": "12345","filename":"original"}
              },
              "options": {}
              }')
  res <- MockPlumberResponse$new()

  queue <- withr::with_envvar(c("USE_MOCK_MODEL" = "false"),
                              test_queue())
  model_submit <- endpoint_model_submit(queue)
  model_result <- endpoint_model_result(queue)

  response <- with_hintr_language(
    "fr",
    model_submit(req, res, data, options, cfg$version_info))

  id <- jsonlite::parse_json(response)$data$id
  queue$queue$task_wait(id)

  result <- model_result(NULL, res, id)
  result <- jsonlite::parse_json(result)
  expect_length(result$errors, 1)
  expect_equal(result$errors[[1]]$detail,
               "Impossible d’ajuster le modèle au niveau du pays. Choisissez un niveau différent.")
})

test_that("failed cancel sends reasonable message", {
  test_redis_available()
  test_mock_model_available()
  ## Create request data
  queue <- test_queue()
  model_cancel <- endpoint_model_cancel(queue)

  id <- ids::random_id()
  req_cancel <- list(postBody = sprintf('{"id": "%s"}', id))
  res <- MockPlumberResponse$new()
  response <- model_cancel(req_cancel, res, id)
  response <- jsonlite::parse_json(response)

  ## TODO: translate the message ideally - requires some work in rrq
  ## though.
  expect_equal(res$status, 400)
  expect_equal(response$status, "failure")
  expect_equal(response$errors[[1]]$error, "FAILED_TO_CANCEL")
  expect_match(response$errors[[1]]$detail,
               "Task [[:xdigit:]]+ is not running \\(MISSING\\)")
  expect_is(response$errors[[1]]$key, "character")
})

test_that("Debug endpoint returns debug information", {
  ## this one needs legit filenames available
  test_redis_available()
  test_mock_model_available()
  ## Create request data
  data <- list(
    pjnz = list(path = file.path("testdata", "Malawi2019.PJNZ"), hash = "12345", filename = "original"),
    shape = list(path = file.path("testdata", "malawi.geojson"), hash = "12345", filename = "original"),
    population = list(path = file.path("testdata", "population.csv"), hash = "12345", filename = "original"),
    survey = list(path = file.path("testdata", "survey.csv"), hash = "12345", filename = "original"),
    programme = list(path = file.path("testdata", "programme.csv"), hash = "12345", filename = "original"),
    anc = NULL
  )
  options = list(a = 1, b = 2)

  req <- list(postBody = '
              "data": {
              "pjnz": {"path":"path/to/file","hash": "12345","filename":"original"}
              "shape":  {"path":"path/to/file","hash": "12345","filename":"original"},
              "population":  {"path":"path/to/file","hash": "12345","filename":"original"},
              "survey":  {"path":"path/to/file","hash": "12345","filename":"original"},
              "programme":  {"path":"path/to/file","hash": "12345","filename":"original"},
              "anc":  {"path":"path/to/file","hash": "12345","filename":"original"}
              },
              "options": {}
              }')

  ## Create mock response
  res <- MockPlumberResponse$new()

  ## Call the endpoint
  queue <- test_queue()
  model_submit <- endpoint_model_submit(queue)
  model_debug <- endpoint_model_debug(queue)

  response <- model_submit(req, res, data, options, cfg$version_info)
  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "success")
  expect_true("id" %in% names(response$data))
  expect_equal(res$status, 200)

  file_with_basename <- function(x) {
    if (is.null(x)) NULL else list(path = basename(x$path), hash = x$hash, filename = x$filename)
  }

  id <- response$data$id
  bin <- model_debug(NULL, NULL, id)
  tmp <- tempfile()
  dest <- tempfile()
  writeBin(bin$bytes, tmp)
  zip::unzip(tmp, exdir = dest)
  expect_equal(dir(dest), id)
  expect_setequal(
    dir(file.path(dest, id)),
    c("data.rds", "files"))
  info <- readRDS(file.path(dest, id, "data.rds"))
  expect_equal(info$objects$options, list(a = 1, b = 2))
  expect_is(info$sessionInfo, "sessionInfo")
  expect_equal(info$objects$data, lapply(data, file_with_basename))
  expect_setequal(
    dir(file.path(dest, id, "files")),
    basename(unlist(lapply(data, function(x) x$path), FALSE, FALSE))
  )
})


test_that("Debug endpoint errors on nonexistant id", {
  test_redis_available()
  res <- MockPlumberResponse$new()
  queue <- test_queue()
  model_debug <- endpoint_model_debug(queue)
  id <- ids::random_id()
  response <- model_debug(list(), res, id)
  response_parsed <- jsonlite::parse_json(response)
  expect_equal(res$status, 400)
  err <- response_parsed$errors[[1]]
  expect_equal(err$error, "INVALID_TASK")
  expect_match(err$detail, "Task '[[:xdigit:]]+' not found")
})
