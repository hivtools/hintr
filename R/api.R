api_build <- function(queue, validate = FALSE, logger = NULL) {
  api <- porcelain::porcelain$new(validate = validate, logger = logger)
  api$handle(endpoint_root())
  api$handle(endpoint_baseline_individual())
  api$handle(endpoint_baseline_combined())
  api$handle(endpoint_validate_survey_programme())
  api$handle(endpoint_input_time_series_plot())
  api$handle(endpoint_model_options())
  api$handle(endpoint_model_options_validate())
  api$handle(endpoint_model_submit(queue))
  api$handle(endpoint_model_status(queue))
  api$handle(endpoint_model_result(queue))
  api$handle(endpoint_model_cancel(queue))
  api$handle(endpoint_model_debug(queue))
  api$handle(endpoint_model_calibrate_options())
  api$handle(endpoint_model_calibrate_submit(queue))
  api$handle(endpoint_model_calibrate_status(queue))
  api$handle(endpoint_model_calibrate_result(queue))
  api$handle(endpoint_model_calibrate_metadata(queue))
  api$handle(endpoint_model_calibrate_data(queue))
  api$handle(endpoint_model_calibrate_plot(queue))
  api$handle(endpoint_comparison_plot(queue))
  api$handle(endpoint_plotting_metadata_iso3())
  api$handle(endpoint_plotting_metadata_default())
  api$handle(endpoint_download_submit(queue))
  api$handle(endpoint_download_status(queue))
  api$handle(endpoint_download_result(queue))
  api$handle(endpoint_download_result_head(queue))
  api$handle(endpoint_adr_metadata(queue))
  api$handle(endpoint_rehydrate_submit(queue))
  api$handle(endpoint_rehydrate_status(queue))
  api$handle(endpoint_rehydrate_result(queue))
  api$handle(endpoint_upload_input(queue))
  api$handle(endpoint_upload_output(queue))
  api$handle(endpoint_prerun(queue))
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
  api_set_language(data, req, res)
}

api_postserialize <- function(data, req, res, value) {
  value <- api_reset_language(data, req, res, value)
  value
}

#' Build and start the API
#'
#' @param queue_id ID of an existing queue to connect to, creates a new one
#' if NULL
#' @param workers Number of workers to spawn
#' @param results_dir The dir for results to be saved to
#' @param inputs_dir THe directory where input files are stored
#' @param log_level The "lgr" log level to use
#'
#' @return Running API
#' @export
api <- function(queue_id = NULL, workers = 2,
                results_dir = tempdir(), inputs_dir = NULL,
                log_level = "info") {
  queue <- Queue$new(queue_id, workers,
                     results_dir = results_dir,
                     inputs_dir = inputs_dir)
  queue$queue$worker_delete_exited()
  logger <- porcelain::porcelain_logger(log_level)
  api_build(queue, logger = logger)
}

api_set_language <- function(data, req, res) {
  if ("accept-language" %in% names(req$HEADERS)) {
    language <- req$HEADERS[["accept-language"]]
    data$reset_language_hintr <- traduire::translator_set_language(language)
    data$reset_language_naomi <-
      traduire::translator_set_language(language, package = "naomi")
    data$reset_language_naomi_options <-
      traduire::translator_set_language(language, package = "naomi.options")
  }
  invisible(NULL)
}

api_reset_language <- function(data, req, res, value) {
  if (!is.null(data$reset_language_naomi)) {
    data$reset_language_naomi()
  }
  if (!is.null(data$reset_language_naomi_options)) {
    data$reset_language_naomi_options()
  }
  if (!is.null(data$reset_language_hintr)) {
    data$reset_language_hintr()
  }
  value
}

endpoint_root <- function() {
  porcelain::porcelain_endpoint$new(
    "GET", "/", root_endpoint,
    returning = porcelain::porcelain_returning_json())
}

endpoint_baseline_individual <- function() {
  ## TODO: Shouldn't have to paste root here but it isn't picking up the
  ## schema directory automatically
  input <- porcelain::porcelain_input_body_json("input",
                                                "ValidateInputRequest.schema",
                                                schema_root())
  response <- porcelain::porcelain_returning_json(
    "ValidateInputResponse.schema", schema_root())
  porcelain::porcelain_endpoint$new("POST",
                                    "/validate/baseline-individual",
                                    validate_baseline,
                                    input,
                                    returning = response)
}

endpoint_baseline_combined <- function() {
  input <- porcelain::porcelain_input_body_json(
    "input", "ValidateBaselineRequest.schema", schema_root())
  response <- porcelain::porcelain_returning_json(
    "ValidateBaselineResponse.schema", schema_root())
  porcelain::porcelain_endpoint$new("POST",
                                    "/validate/baseline-combined",
                                    validate_baseline_combined,
                                    input,
                                    returning = response)
}

endpoint_validate_survey_programme <- function() {
  input_body <- porcelain::porcelain_input_body_json(
    "input", "ValidateSurveyAndProgrammeRequest.schema", schema_root())
  input_query <- porcelain::porcelain_input_query(strict = "logical")
  response <- porcelain::porcelain_returning_json(
    "ValidateInputResponse.schema", schema_root())
  porcelain::porcelain_endpoint$new("POST",
                                    "/validate/survey-and-programme",
                                    validate_survey_programme,
                                    input_body,
                                    input_query,
                                    returning = response)
}

endpoint_input_time_series_plot <- function() {
  input <- porcelain::porcelain_input_body_json(
    "input", "InputTimeSeriesRequest.schema", schema_root())
  response <- porcelain::porcelain_returning_json(
    "InputTimeSeriesResponse.schema", schema_root())
  porcelain::porcelain_endpoint$new("POST",
                                    "/chart-data/input-time-series/<type>",
                                    input_time_series,
                                    input,
                                    returning = response)
}

returning_json_version <- function(schema = NULL, root = NULL,
                                   status_code = 200L) {
  ## This is the same as porcelain::porcelain_returning_json except we
  ## override the process function to also add version info along side the
  ## data
  returning  <- porcelain::porcelain_returning_json(schema, root, status_code)
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
  input <- porcelain::porcelain_input_body_json("input",
                                                "ModelRunOptionsRequest.schema",
                                                schema_root())
  response <- returning_json_version("ModelRunOptions.schema", schema_root())
  porcelain::porcelain_endpoint$new("POST",
                                    "/model/options",
                                    model_options,
                                    input,
                                    returning = response)
}

endpoint_model_options_validate <- function() {
  input <- porcelain::porcelain_input_body_json(
    "input", "ModelOptionsValidateRequest.schema", schema_root())
  response <- porcelain::porcelain_returning_json("ModelOptionsValidate.schema",
                                                  schema_root())
  porcelain::porcelain_endpoint$new("POST",
                                    "/validate/options",
                                    model_options_validate,
                                    input,
                                    returning = response)
}


endpoint_model_submit <- function(queue) {
  input <- porcelain::porcelain_input_body_json("input",
                                                "ModelSubmitRequest.schema",
                                                schema_root())
  response <- porcelain::porcelain_returning_json("ModelSubmitResponse.schema",
                                                  schema_root())
  porcelain::porcelain_endpoint$new("POST",
                                    "/model/submit",
                                    submit_model(queue),
                                    input,
                                    returning = response)
}

endpoint_model_status <- function(queue) {
  response <- porcelain::porcelain_returning_json("ModelStatusResponse.schema",
                                                  schema_root())
  porcelain::porcelain_endpoint$new("GET",
                                    "/model/status/<id>",
                                    queue_status(queue),
                                    returning = response)
}

endpoint_model_result <- function(queue) {
  response <- porcelain::porcelain_returning_json("ModelResultResponse.schema",
                                                  schema_root())
  porcelain::porcelain_endpoint$new("GET",
                                    "/model/result/<id>",
                                    model_result(queue),
                                    returning = response)
}

endpoint_model_cancel <- function(queue) {
  response <- porcelain::porcelain_returning_json("ModelCancelResponse.schema",
                                                  schema_root())
  porcelain::porcelain_endpoint$new("POST",
                                    "/model/cancel/<id>",
                                    model_cancel(queue),
                                    returning = response)
}

endpoint_model_debug <- function(queue) {
  porcelain::porcelain_endpoint$new(
    "GET", "/model/debug/<id>", download_model_debug(queue),
    returning = porcelain::porcelain_returning_binary())
}

endpoint_model_calibrate_options <- function() {
  response <- returning_json_version("ModelRunOptions.schema", schema_root())
  porcelain::porcelain_endpoint$new("POST",
                                    "/calibrate/options/<iso3>",
                                    calibration_options,
                                    returning = response)
}

endpoint_model_calibrate_submit <- function(queue) {
  input <- porcelain::porcelain_input_body_json("input",
                                                "CalibrateSubmitRequest.schema",
                                                schema_root())
  response <- porcelain::porcelain_returning_json(
    "CalibrateSubmitResponse.schema", schema_root())
  porcelain::porcelain_endpoint$new("POST",
                                    "/calibrate/submit/<id>",
                                    submit_calibrate(queue),
                                    input,
                                    returning = response)
}

endpoint_model_calibrate_status <- function(queue) {
  response <- porcelain::porcelain_returning_json(
    "CalibrateStatusResponse.schema", schema_root())
  porcelain::porcelain_endpoint$new("GET",
                                    "/calibrate/status/<id>",
                                    queue_status(queue),
                                    returning = response)
}

endpoint_model_calibrate_result <- function(queue) {
  response <- porcelain::porcelain_returning_json(
    "CalibrateResultResponse.schema", schema_root())
  porcelain::porcelain_endpoint$new("GET",
                                    "/calibrate/result/<id>",
                                    calibrate_result(queue),
                                    returning = response)
}

endpoint_model_calibrate_metadata <- function(queue) {
  response <- porcelain::porcelain_returning_json(
    "CalibrateMetadataResponse.schema", schema_root())
  porcelain::porcelain_endpoint$new("GET",
                                    "/calibrate/result/metadata/<id>",
                                    calibrate_metadata(queue),
                                    returning = response)
}

endpoint_model_calibrate_data <- function(queue) {
  response <- porcelain::porcelain_returning_json(
    "CalibrateDataResponse.schema", schema_root())
  porcelain::porcelain_endpoint$new("GET",
                                    "/calibrate/result/data/<id>",
                                    calibrate_data(queue),
                                    returning = response)
}

endpoint_model_calibrate_plot <- function(queue) {
  response <- porcelain::porcelain_returning_json(
    "CalibratePlotResponse.schema", schema_root())
  porcelain::porcelain_endpoint$new("GET",
                                    "/calibrate/plot/<id>",
                                    calibrate_plot(queue),
                                    returning = response)
}

endpoint_comparison_plot <- function(queue) {
  response <- porcelain::porcelain_returning_json(
    "ComparisonPlotResponse.schema", schema_root())
  porcelain::porcelain_endpoint$new("GET",
                                    "/comparison/plot/<id>",
                                    comparison_plot(queue),
                                    returning = response)
}

endpoint_plotting_metadata_iso3 <- function() {
  endpoint_plotting_metadata("/meta/plotting/<iso3>")
}

endpoint_plotting_metadata_default <- function() {
  endpoint_plotting_metadata("/meta/plotting")
}

endpoint_plotting_metadata <- function(path) {
  response <- porcelain::porcelain_returning_json(
    "PlottingMetadataResponse.schema", schema_root())
  porcelain::porcelain_endpoint$new("GET",
                                    path,
                                    plotting_metadata,
                                    returning = response)
}

## Return same headers as binary returning but ensure no body is returned.
returning_binary_head <- function(status_code = 200L) {
  porcelain::porcelain_returning("application/octet-stream",
                                 process = function(data) NULL,
                                 validate = function(body) TRUE)
}

endpoint_download_submit <- function(queue) {
  input <- porcelain::porcelain_input_body_json("input",
                                                "DownloadSubmitRequest.schema",
                                                schema_root())
  response <- porcelain::porcelain_returning_json(
    "DownloadSubmitResponse.schema", schema_root())
  porcelain::porcelain_endpoint$new("POST",
                                    "/download/submit/<type>/<id>",
                                    download_submit(queue),
                                    input,
                                    returning = response)
}

endpoint_download_status <- function(queue) {
  response <- porcelain::porcelain_returning_json(
    "DownloadStatusResponse.schema", schema_root())
  porcelain::porcelain_endpoint$new("GET",
                                    "/download/status/<id>",
                                    queue_status(queue),
                                    returning = response)
}

endpoint_download_result <- function(queue) {
  porcelain::porcelain_endpoint$new(
    "GET",
    "/download/result/<id>",
    download_result(queue),
    returning = porcelain::porcelain_returning_binary())
}

endpoint_download_result_head <- function(queue) {
  porcelain::porcelain_endpoint$new("HEAD",
                                    "/download/result/<id>",
                                    download_result(queue),
                                    returning = returning_binary_head(),
                                    validate = FALSE)
}

endpoint_adr_metadata <- function(queue) {
  response <- porcelain::porcelain_returning_json(
    "AdrMetadataResponse.schema", schema_root())
  porcelain::porcelain_endpoint$new("GET",
                                    "/meta/adr/<id>",
                                    adr_metadata(queue),
                                    returning = response)
}

endpoint_upload_input <- function(queue) {
  input <- porcelain::porcelain_input_body_binary("file")
  response <- porcelain::porcelain_returning_json(
    "File.schema", schema_root())
  porcelain::porcelain_endpoint$new("POST",
                                    "/internal/upload/input/<filename>",
                                    upload_file(queue$inputs_dir),
                                    input,
                                    returning = response)
}

endpoint_upload_output <- function(queue) {
  input <- porcelain::porcelain_input_body_binary("file")
  response <- porcelain::porcelain_returning_json(
    "File.schema", schema_root())
  porcelain::porcelain_endpoint$new("POST",
                                    "/internal/upload/result/<filename>",
                                    upload_file(queue$results_dir),
                                    input,
                                    returning = response)
}

endpoint_prerun <- function(queue) {
  input <- porcelain::porcelain_input_body_json("input",
                                                "PrerunRequest.schema",
                                                schema_root())
  response <- porcelain::porcelain_returning_json(
    "ProjectState.schema.json", schema_root())
  porcelain::porcelain_endpoint$new("POST",
                                    "/internal/prerun",
                                    prerun(queue),
                                    input,
                                    returning = response)
}

endpoint_hintr_version <- function() {
  response <- porcelain::porcelain_returning_json("HintrVersionResponse.schema",
                                                  schema_root())
  porcelain::porcelain_endpoint$new("GET",
                                    "/hintr/version",
                                    function() cfg$version_info,
                                    returning = response)
}

endpoint_hintr_worker_status <- function(queue) {
  response <- porcelain::porcelain_returning_json("HintrWorkerStatus.schema",
                                                  schema_root())
  porcelain::porcelain_endpoint$new("GET",
                                    "/hintr/worker/status",
                                    worker_status(queue),
                                    returning = response)
}

endpoint_hintr_stop <- function(queue) {
  ## This endpoint calls hintr_stop which kills any workers and then calls stop.
  ## It will never return anything so this won't ever be called in production,
  ## it exists only so that when we mock hintr_stop this returns without errors
  ## so we can effectively test.
  returning <- porcelain::porcelain_returning(
    content_type = "application/json",
    process = function(data) json_null(),
    validate = function(body) TRUE)
  porcelain::porcelain_endpoint$new("POST",
                                    "/hintr/stop",
                                    hintr_stop(queue),
                                    returning = returning,
                                    validate = FALSE)
}

endpoint_rehydrate_submit <- function(queue) {
  input <- porcelain::porcelain_input_body_json(
    "input", "ProjectRehydrateSubmitRequest.schema", schema_root())
  response <- porcelain::porcelain_returning_json(
    "ProjectRehydrateSubmitResponse.schema", schema_root())
  porcelain::porcelain_endpoint$new("POST",
                                    "/rehydrate/submit",
                                    rehydrate_submit(queue),
                                    input,
                                    returning = response)
}

endpoint_rehydrate_status <- function(queue) {
  response <- porcelain::porcelain_returning_json(
    "ProjectRehydrateStatusResponse.schema", schema_root())
  porcelain::porcelain_endpoint$new("GET",
                                    "/rehydrate/status/<id>",
                                    queue_status(queue),
                                    returning = response)
}

endpoint_rehydrate_result <- function(queue) {
  response <- porcelain::porcelain_returning_json(
    "ProjectRehydrateResultResponse.schema", schema_root())
  porcelain::porcelain_endpoint$new("GET",
                                    "/rehydrate/result/<id>",
                                    rehydrate_result(queue),
                                    returning = response)
}
