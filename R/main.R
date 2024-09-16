main_api_args <- function(args = commandArgs(TRUE)) {
  usage <- "Usage:
  hintr_api [options] [<queue_id>]

Options:
--workers=N               Number of workers to spawn [default: 2]
--port=PORT               Port to use [default: 8888]
--results-dir=PATH        Directory to store model results in
--prerun-dir=PATH         Directory to find prerun results in
--inputs-dir=PATH         Directory to find input files in
--health-check-interval=N Interval in seconds, after which next time redis connection is used the connection will be reset. 0 for no reconnection. [default: 0]"

  validate_path <- function(path) {
    if (is.null(path)) {
      path <- tempfile()
      dir.create(path, FALSE, TRUE)
    }
    path
  }
  dat <- docopt_parse(usage, args)
  list(port = as.integer(dat$port),
       queue_id = dat$queue_id,
       workers = as.integer(dat$workers),
       results_dir = validate_path(dat[["results_dir"]]),
       inputs_dir = validate_path(dat[["inputs_dir"]]),
       health_check_interval = as.integer(dat$health_check_interval))
}

main_api <- function(args = commandArgs(TRUE)) {
  # nocov start
  dat <- main_api_args(args)
  api <- api(dat$queue_id, dat$workers, dat$results_dir, dat$inputs_dir,
             health_check_interval = dat$health_check_interval)
  api$run(host = "0.0.0.0", port = dat$port)
  # nocov end
}

main_worker_args <- function(args = commandArgs(TRUE)) {
  usage <- "Usage:
hintr_worker [options] [<queue_id>]

Options:
--calibrate-only  Start a worker which will only run calibration tasks"
  dat <- docopt_parse(usage, args)
  list(queue_id = dat$queue_id,
       calibrate_only = dat$calibrate_only)
}

main_worker <- function(args = commandArgs(TRUE)) {
  # nocov start
  args <- main_worker_args(args)
  worker_config <- "localhost"
  if (args$calibrate_only) {
    worker_config <- "calibrate_only"
  }
  worker <- rrq_worker_new(hintr_queue_id(args$queue_id, TRUE),
                           name_config = worker_config)
  worker$loop()
  invisible(TRUE)
  # nocov end
}

rrq_worker_new <- function(...) {
  # nocov start
  rrq::rrq_worker$new(...)
  # nocov end
}

main_worker_single_job_args <- function(args = commandArgs(TRUE)) {
  usage <- "Usage:
hintr_worker_single_job [options] [<queue_id>]

Options:
--fit-only  Start a worker which will only run model fit"
  dat <- docopt_parse(usage, args)
  list(queue_id = dat$queue_id,
       fit_only = dat$fit_only)
}

main_worker_single_job <- function(args = commandArgs(TRUE)) {
  # nocov start
  args <- main_worker_single_job_args(args)
  worker_config <- "localhost"
  if (args$fit_only) {
    worker_config <- "fit_only"
  }
  worker_single_job(hintr_queue_id(args$queue_id, TRUE), worker_config)
  # nocov end
}

worker_single_job <- function(queue_id, config) {
  worker <- rrq_worker_new(queue_id, name_config = config)
  worker$step(immediate = TRUE)
  invisible(TRUE)
}

docopt_parse <- function(usage, args) {
  dat <- docopt::docopt(usage, args)
  names(dat) <- gsub("-", "_", names(dat), fixed = TRUE)
  dat
}
