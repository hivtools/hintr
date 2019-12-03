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
  dat <- docopt::docopt(usage, args)
  list(port = as.integer(dat$port),
       queue_id = dat$queue_id,
       workers = as.integer(dat$workers),
       results_dir = validate_path(dat[["results-dir"]]),
       prerun_dir = validate_path(dat[["prerun-dir"]]))
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
  dat <- docopt::docopt(usage, args)
  list(queue_id = dat$queue_id)
}

main_worker <- function(args = commandArgs(TRUE)) {
  rrq::rrq_worker(hintr_queue_id(main_worker_args(args)$queue_id, TRUE)) # nocov
}
