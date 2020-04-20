api_build <- function(queue) {
  pr <- plumber::plumber$new()
  pr$handle("POST", "/validate/baseline-individual", endpoint_validate_baseline,
            serializer = serializer_json_hintr())
  pr$handle("POST", "/validate/baseline-combined", endpoint_validate_baseline_combined,
            serializer = serializer_json_hintr())
  pr$handle("POST", "/validate/survey-and-programme", endpoint_validate_survey_programme,
            serializer = serializer_json_hintr())
  pr$handle("POST", "/model/options", endpoint_model_options,
            serializer = serializer_json_hintr())
  pr$handle("POST", "/validate/options", endpoint_model_options_validate,
            serializer = serializer_json_hintr())
  pr$handle("POST", "/model/submit", endpoint_model_submit(queue),
            serializer = serializer_json_hintr())
  pr$handle("GET", "/model/status/<id>", endpoint_model_status(queue),
            serializer = serializer_json_hintr())
  pr$handle("GET", "/model/result/<id>", endpoint_model_result(queue),
            serializer = serializer_json_hintr())
  pr$handle("POST", "/model/cancel/<id>", endpoint_model_cancel(queue),
            serializer = serializer_json_hintr())
  pr$handle("GET", "/model/debug/<id>", endpoint_model_debug(queue),
            serializer = serializer_zip("naomi_debug"))
  pr$handle("GET", "/meta/plotting/<iso3>", endpoint_plotting_metadata,
            serializer = serializer_json_hintr())
  pr$handle("GET", "/download/spectrum/<id>", endpoint_download_spectrum(queue),
            serializer = serializer_zip("naomi_spectrum_digest"))
  pr$handle("HEAD", "/download/spectrum/<id>", endpoint_download_spectrum(queue),
            serializer = serializer_zip("naomi_spectrum_digest"))
  pr$handle("GET", "/download/summary/<id>", endpoint_download_summary(queue),
            serializer = serializer_zip("naomi_coarse_age_groups"))
  pr$handle("HEAD", "/download/summary/<id>", endpoint_download_summary(queue),
            serializer = serializer_zip("naomi_coarse_age_groups"))
  pr$handle("GET", "/hintr/version", endpoint_hintr_version,
            serializer = serializer_json_hintr())
  pr$handle("GET", "/hintr/worker/status", endpoint_hintr_worker_status(queue),
            serializer = serializer_json_hintr())
  pr$handle("POST", "/hintr/stop", endpoint_hintr_stop(queue))
  pr$handle("GET", "/", endpoint_root)

  pr$registerHook("preroute", api_preroute)
  pr$registerHook("postserialize", api_postserialize)
  pr$set404Handler(hintr_404_handler)
  pr$setErrorHandler(hintr_error_handler)

  pr
}

api_preroute <- function(data, req, res, value) {
  api_log_start(data, req, res)
  api_set_language(data, req, res)
}

api_postserialize <- function(data, req, res, value) {
  value <- api_log_end(data, req, res, value)
  value <- api_reset_language(data, req, res, value)
  value
}

api_run <- function(pr, port = 8888) {
  pr$run(host = "0.0.0.0", port = port) # nocov
}

api <- function(port = 8888, queue_id = NULL, workers = 2,
                results_dir = tempdir(), prerun_dir = NULL) {
  # nocov start
  queue <- Queue$new(queue_id, workers, results_dir = results_dir,
                     prerun_dir = prerun_dir)
  api_run(api_build(queue), port)
  # nocov end
}

api_log_start <- function(data, req, res) {
  api_log(sprintf("%s %s", req$REQUEST_METHOD, req$PATH_INFO))
}

api_log_end <- function(data, req, res, value) {
  if (is.raw(res$body)) {
    size <- length(res$body)
  } else {
    size <- nchar(res$body)
  }
  if (res$status >= 400 &&
      identical(res$headers[["Content-Type"]], "application/json")) {
    dat <- jsonlite::parse_json(res$body)
    for (e in dat$errors) {
      if (!is.null(e$key)) {
        api_log(sprintf("error-key: %s", e$key))
        api_log(sprintf("error-detail: %s", e$detail))
        if (!is.null(e$trace)) {
          trace <- sub("\n", " ", vcapply(e$trace, identity))
          api_log(sprintf("error-trace: %s", trace))
        }
      }
    }
  }
  api_log(sprintf("`--> %d (%d bytes)", res$status, size))
  value
}

# We can route this via some check for enabling/disabling logging later
api_log <- function(msg) {
  message(paste(sprintf("[%s] %s", Sys.time(), msg), collapse = "\n"))
}

api_set_language <- function(data, req, res) {
  if ("accept-language" %in% names(req$HEADERS)) {
    language <- req$HEADERS[["accept-language"]]
    data$reset_language_hintr <- traduire::translator_set_language(language)
    data$reset_language_naomi <-
      traduire::translator_set_language(language, package = "naomi")
  }
  invisible(NULL)
}

api_reset_language <- function(data, req, res, value) {
  if (!is.null(data$reset_language_naomi)) {
    data$reset_language_naomi()
  }
  if (!is.null(data$reset_language_hintr)) {
    data$reset_language_hintr()
  }
  value
}
