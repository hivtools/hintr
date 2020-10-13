api_build <- function(queue) {
  api <- pkgapi::pkgapi$new()
  api$handle(endpoint_root())
  api$handle(endpoint_baseline_individual())
  api$handle(endpoint_baseline_combined())
  api$handle(endpoint_validate_survey_programme())
  api$handle(endpoint_model_options())
  api$handle(endpoint_model_options_validate())
  api$handle(endpoint_model_submit(queue))
  api$handle(endpoint_model_status(queue))
  api$handle(endpoint_model_result(queue))
  api$handle(endpoint_model_cancel(queue))
  api$handle(endpoint_model_debug(queue))
  api$handle(endpoint_model_calibration_options())
  api$handle(endpoint_model_calibrate(queue))
  api$handle(endpoint_plotting_metadata())
  api$handle(endpoint_download_spectrum(queue))
  api$handle(endpoint_download_spectrum_head(queue))
  api$handle(endpoint_download_coarse_output(queue))
  api$handle(endpoint_download_coarse_output_head(queue))
  api$handle(endpoint_hintr_version())
  api$handle(endpoint_hintr_worker_status(queue))
  api$handle(endpoint_hintr_stop(queue))
  api$registerHook("preroute", api_preroute)
  api$registerHook("postserialize", api_postserialize)
  api$set404Handler(hintr_404_handler)
  api$setDocs(FALSE)
  api
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

#' Build and start the API
#'
#' @param port Port for API
#' @param queue_id ID of an existing queue to connect to, creates a new one
#' if NULL
#' @param workers Number of workers to spawn
#' @param results_dir The dir for results to be saved to
#' @param prerun_dir The directory to store prerun results
#'
#' @return Running API
#' @export
api <- function(port = 8888, queue_id = NULL, workers = 2,
                results_dir = tempdir(), prerun_dir = NULL) {
  # nocov start
  queue <- Queue$new(queue_id, workers, results_dir = results_dir,
                     prerun_dir = prerun_dir)
  api <- api_build(queue)
  api$run(host = "0.0.0.0", port = port)
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

endpoint_root <- function() {
  pkgapi::pkgapi_endpoint$new("GET",
                              "/",
                              root_endpoint,
                              returning = pkgapi::pkgapi_returning_json())
}

endpoint_baseline_individual <- function() {
  ## TODO: Shouldn't have to paste root here but it isn't picking up the
  ## schema directory automatically
  input <- pkgapi::pkgapi_input_body_json("input",
                                          "ValidateInputRequest.schema",
                                          schema_root())
  response <- pkgapi::pkgapi_returning_json("ValidateInputResponse.schema",
                                            schema_root())
  pkgapi::pkgapi_endpoint$new("POST",
                              "/validate/baseline-individual",
                              validate_baseline,
                              input,
                              returning = response,
                              validate = TRUE)
}

endpoint_baseline_combined <- function() {
  input <- pkgapi::pkgapi_input_body_json("input",
                                          "ValidateBaselineRequest.schema",
                                          schema_root())
  response <- pkgapi::pkgapi_returning_json("ValidateBaselineResponse.schema",
                                            schema_root())
  pkgapi::pkgapi_endpoint$new("POST",
                              "/validate/baseline-combined",
                              validate_baseline_combined,
                              input,
                              returning = response,
                              validate = TRUE)
}

endpoint_validate_survey_programme <- function() {
  input <- pkgapi::pkgapi_input_body_json(
    "input", "ValidateSurveyAndProgrammeRequest.schema", schema_root())
  response <- pkgapi::pkgapi_returning_json("ValidateInputResponse.schema",
                                            schema_root())
  pkgapi::pkgapi_endpoint$new("POST",
                              "/validate/survey-and-programme",
                              validate_survey_programme,
                              input,
                              returning = response,
                              validate = TRUE)
}

returning_json_version <- function(schema = NULL, root = NULL,
                                   status_code = 200L) {
  ## This is the same as pkgapi::pkgapi_returning_json except we
  ## override the process function to also add version info along side the
  ## data
  returning  <- pkgapi::pkgapi_returning_json(schema, root, status_code)
  response_success <- function(data) {
    list(
      status = jsonlite::unbox("success"),
      errors = json_null(),
      data = data,
      version = cfg$version_info
    )
  }
  returning$process <- function(data) {
    as.character(to_json(response_success(data)))
  }
  returning
}

endpoint_model_options <- function() {
  input <- pkgapi::pkgapi_input_body_json("input",
                                          "ModelRunOptionsRequest.schema",
                                          schema_root())
  response <- returning_json_version("ModelRunOptions.schema", schema_root())
  pkgapi::pkgapi_endpoint$new("POST",
                              "/model/options",
                              model_options,
                              input,
                              returning = response,
                              validate = TRUE)
}

endpoint_model_options_validate <- function() {
  input <- pkgapi::pkgapi_input_body_json("input",
                                          "ModelOptionsValidateRequest.schema",
                                          schema_root())
  response <- pkgapi::pkgapi_returning_json("ModelOptionsValidate.schema",
                                            schema_root())
  pkgapi::pkgapi_endpoint$new("POST",
                              "/validate/options",
                              model_options_validate,
                              input,
                              returning = response,
                              validate = TRUE)
}


endpoint_model_submit <- function(queue) {
  input <- pkgapi::pkgapi_input_body_json("input",
                                          "ModelSubmitRequest.schema",
                                          schema_root())
  response <- pkgapi::pkgapi_returning_json("ModelSubmitResponse.schema",
                                            schema_root())
  pkgapi::pkgapi_endpoint$new("POST",
                              "/model/submit",
                              submit_model(queue),
                              input,
                              returning = response,
                              validate = TRUE)
}

endpoint_model_status <- function(queue) {
  response <- pkgapi::pkgapi_returning_json("ModelStatusResponse.schema",
                                            schema_root())
  pkgapi::pkgapi_endpoint$new("GET",
                              "/model/status/<id>",
                              model_status(queue),
                              returning = response,
                              validate = TRUE)
}

endpoint_model_result <- function(queue) {
  response <- pkgapi::pkgapi_returning_json("ModelResultResponse.schema",
                                            schema_root())
  pkgapi::pkgapi_endpoint$new("GET",
                              "/model/result/<id>",
                              model_result(queue),
                              returning = response,
                              validate = TRUE)
}

endpoint_model_cancel <- function(queue) {
  response <- pkgapi::pkgapi_returning_json("ModelCancelResponse.schema",
                                            schema_root())
  pkgapi::pkgapi_endpoint$new("POST",
                              "/model/cancel/<id>",
                              model_cancel(queue),
                              returning = response,
                              validate = TRUE)
}

endpoint_model_debug <- function(queue) {
  pkgapi::pkgapi_endpoint$new("GET",
                              "/model/debug/<id>",
                              download_model_debug(queue),
                              returning = pkgapi::pkgapi_returning_binary())
}

endpoint_model_calibration_options <- function() {
  response <- returning_json_version("ModelRunOptions.schema", schema_root())
  pkgapi::pkgapi_endpoint$new("POST",
                              "/model/calibration-options",
                              calibration_options,
                              returning = response,
                              validate = TRUE)
}

endpoint_model_calibrate <- function(queue) {
  input <- pkgapi::pkgapi_input_body_json("input",
                                          "ModelCalibrateRequest.schema",
                                          schema_root())
  response <- pkgapi::pkgapi_returning_json("ModelResultResponse.schema",
                                            schema_root())
  pkgapi::pkgapi_endpoint$new("POST",
                              "/model/calibrate/<id>",
                              model_calibrate(queue),
                              input,
                              returning = response,
                              validate = TRUE)
}

endpoint_plotting_metadata <- function() {
  response <- pkgapi::pkgapi_returning_json("PlottingMetadataResponse.schema",
                                            schema_root())
  pkgapi::pkgapi_endpoint$new("GET",
                              "/meta/plotting/<iso3>",
                              plotting_metadata,
                              returning = response,
                              validate = TRUE)
}

## Return same headers as binary returning but ensure no body is returned.
returning_binary_head <- function(status_code = 200L) {
  pkgapi::pkgapi_returning("application/octet-stream",
                           process = function(data) NULL,
                           validate = function(body) TRUE)
}

endpoint_download_spectrum <- function(queue) {
  pkgapi::pkgapi_endpoint$new("GET",
                              "/download/spectrum/<id>",
                              download_spectrum(queue),
                              returning = pkgapi::pkgapi_returning_binary())
}

endpoint_download_spectrum_head <- function(queue) {
  pkgapi::pkgapi_endpoint$new("HEAD",
                              "/download/spectrum/<id>",
                              download_spectrum(queue),
                              returning = returning_binary_head(),
                              validate = FALSE)
}

endpoint_download_coarse_output <- function(queue) {
  pkgapi::pkgapi_endpoint$new("GET",
                              "/download/coarse-output/<id>",
                              download_coarse_output(queue),
                              returning = pkgapi::pkgapi_returning_binary())
}

endpoint_download_coarse_output_head <- function(queue) {
  pkgapi::pkgapi_endpoint$new("HEAD",
                              "/download/coarse-output/<id>",
                              download_coarse_output(queue),
                              returning = returning_binary_head(),
                              validate = FALSE)
}

endpoint_hintr_version <- function() {
  response <- pkgapi::pkgapi_returning_json("HintrVersionResponse.schema",
                                            schema_root())
  pkgapi::pkgapi_endpoint$new("GET",
                              "/hintr/version",
                              function() cfg$version_info,
                              returning = response,
                              validate = TRUE)
}

endpoint_hintr_worker_status <- function(queue) {
  response <- pkgapi::pkgapi_returning_json("HintrWorkerStatus.schema",
                                            schema_root())
  pkgapi::pkgapi_endpoint$new("GET",
                              "/hintr/worker/status",
                              worker_status(queue),
                              returning = response,
                              validate = TRUE)
}

endpoint_hintr_stop <- function(queue) {
  ## This endpoint calls hintr_stop which kills any workers and then calls stop.
  ## It will never return anything so this won't ever be called in production,
  ## it exists only so that when we mock hintr_stop this returns without errors
  ## so we can effectively test.
  returning <- pkgapi::pkgapi_returning(content_type = "application/json",
                                        process = function(data) json_null(),
                                        validate = function(body) TRUE)
  pkgapi::pkgapi_endpoint$new("POST",
                              "/hintr/stop",
                              hintr_stop(queue),
                              returning = returning,
                              validate = FALSE)
}
