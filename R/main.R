main_api_args <- function(args = commandArgs(TRUE)) {
  usage <- "Usage:
  hintr_api [options] [<queue_id>]

Options:
--workers=N         Number of workers to spawn [default: 2]
--port=PORT         Port to use [default: 8888]
--results-dir=PATH  Directory to store model results in
--prerun-dir=PATH   Directory to find prerun results in"

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
       prerun_dir = validate_path(dat[["prerun_dir"]]))
}

main_api <- function(args = commandArgs(TRUE)) {
  # nocov start
  dat <- main_api_args(args)
  api(dat$port, dat$queue_id, dat$workers, dat$results_dir, dat$prerun_dir)
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
  worker <- rrq::rrq_worker_from_config(hintr_queue_id(args$queue_id, TRUE),
                                        worker_config = worker_config)
  worker$loop()
  invisible(TRUE)
  # nocov end
}

main_import_prerun <- function(args = commandArgs(TRUE)) {
  usage <- "Usage:
  hintr-import-prerun <prerun> <path> [options]

Options:
--output=PATH    Path to output file [default: output.rds]
--spectrum=PATH  Path to spectrum file [default: spectrum.zip]
--coarse-output=PATH   Path to coarse-output file [default: coarse-output.zip]
--summary-report=PATH  Path to summary-report file [default: summary-report.html]
--calibration=PATH     Path to calibration file [default: calibration.rds]"
  args <- docopt_parse(usage, args)
  h <- prerun_import(args$prerun, args$path, args$output, args$spectrum,
                     args$coarse_output, args$summary_report, args$calibration)
  message(sprintf("Imported data as '%s'", h))
}

docopt_parse <- function(usage, args) {
  dat <- docopt::docopt(usage, args)
  names(dat) <- gsub("-", "_", names(dat), fixed = TRUE)
  dat
}
