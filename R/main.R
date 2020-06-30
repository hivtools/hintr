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
hintr_worker [<queue_id>]"
  dat <- docopt_parse(usage, args)
  list(queue_id = dat$queue_id)
}

main_worker <- function(args = commandArgs(TRUE)) {
  # nocov start
  rrq::rrq_worker(hintr_queue_id(main_worker_args(args)$queue_id, TRUE),
                  heartbeat_period = 10)
  # nocov end
}

main_import_prerun <- function(args = commandArgs(TRUE)) {
  usage <- "Usage:
  hintr-import-prerun <prerun> <path> [options]

Options:
--output=PATH    Path to output file [default: output.rds]
--spectrum=PATH  Path to spectrum file [default: spectrum.zip]
--summary=PATH   Path to summary file [default: summary.zip]"
  args <- docopt_parse(usage, args)
  h <- prerun_import(args$prerun, args$path,
                     args$output, args$spectrum, args$summary)
  message(sprintf("Imported data as '%s'", h))
}

docopt_parse <- function(usage, args) {
  dat <- docopt::docopt(usage, args)
  names(dat) <- gsub("-", "_", names(dat), fixed = TRUE)
  dat
}
