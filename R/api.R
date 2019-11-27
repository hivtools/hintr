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
  pr$handle("POST", "/model/submit", endpoint_model_submit(queue),
            serializer = serializer_json_hintr())
  pr$handle("GET", "/model/status/<id>", endpoint_model_status(queue),
            serializer = serializer_json_hintr())
  pr$handle("GET", "/model/result/<id>", endpoint_model_result(queue),
            serializer = serializer_json_hintr())
  pr$handle("GET", "/meta/plotting/<iso3>", endpoint_plotting_metadata,
            serializer = serializer_json_hintr())
  pr$handle("GET", "/download/spectrum/<id>", endpoint_download_spectrum(queue),
            serializer = serializer_zip("naomi_spectrum_digest"))
  pr$handle("HEAD", "/download/spectrum/<id>", endpoint_download_spectrum(queue),
            serializer = serializer_zip("naomi_spectrum_digest"))
  pr$handle("GET", "/download/summary/<id>", endpoint_download_summary(queue),
            serializer = serializer_zip("naomi_summary"))
  pr$handle("HEAD", "/download/summary/<id>", endpoint_download_summary(queue),
            serializer = serializer_zip("naomi_summary"))
  pr$handle("GET", "/hintr/version", endpoint_hintr_version,
            serializer = serializer_json_hintr())
  pr$handle("GET", "/hintr/worker/status", endpoint_hintr_worker_status(queue),
            serializer = serializer_json_hintr())
  pr$handle("POST", "/hintr/stop", endpoint_hintr_stop(queue))
  pr$handle("GET", "/", endpoint_root)

  pr$registerHook("preroute", api_log_start)
  pr$registerHook("postserialize", api_log_end)
  pr$set404Handler(hintr_404_handler)
  pr$setErrorHandler(hintr_error_handler)

  pr
}

api_run <- function(pr, port = 8888) {
  pr$run(host = "0.0.0.0", port = port) # nocov
}

api <- function(port = 8888, queue_id = NULL, workers = 2,
                results_dir = tempdir()) {
  queue <- Queue$new(queue_id, workers, results_dir = results_dir) # nocov
  api_run(api_build(queue), port) # nocov
}

api_log_start <- function(data, req, res) {
  api_log("%s %s", req$REQUEST_METHOD, req$PATH_INFO)
}

api_log_end <- function(data, req, res, value) {
  if (is.raw(value$body)) {
    size <- length(value$body)
  } else {
    size <- nchar(value$body)
  }
  api_log("`--> %d (%d bytes)", value$status, size)
  value
}

# We can route this via some check for enabling/disabling logging later
api_log <- function(fmt, ...) {
  message(sprintf("[%s] %s", Sys.time(), sprintf(fmt, ...)))
}
