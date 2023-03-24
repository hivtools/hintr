validation_api_build <- function(validate = FALSE, logger = NULL) {
  api <- porcelain::porcelain$new(validate = validate, logger = logger)
  api$handle(endpoint_root())
  api$handle(endpoint_baseline_individual())
  api$handle(endpoint_baseline_combined())
  api$handle(endpoint_validate_survey_programme())
  api$registerHook("preroute", api_preroute)
  api$registerHook("postserialize", api_postserialize)
  api$set404Handler(hintr_404_handler)
  api$setDocs(FALSE)
  api
}

#' Build and start the validation API
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
validation_api <- function(log_level = "info") {
  logger <- porcelain::porcelain_logger(log_level)
  validation_api_build(logger = logger)
}


validation_api_args <- function(args = commandArgs(TRUE)) {
  usage <- "Usage:
  hintr_api [options]

Options:
--port=PORT         Port to use [default: 8888]"

  dat <- docopt_parse(usage, args)
  list(port = as.integer(dat$port))
}

validation_api_main <- function(args = commandArgs(TRUE)) {
  # nocov start
  dat <- validation_api_args(args)
  api <- validation_api()
  api$run(host = "0.0.0.0", port = dat$port)
  # nocov end
}
