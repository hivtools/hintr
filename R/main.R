main_api_args <- function(args = commandArgs(TRUE)) {
  usage <- "Usage:
  hintr_api [options] [<queue_id>]

Options:
--workers=N              Number of workers to spawn [default: 2]
--port=PORT              Port to use [default: 8888]
--results_dir=RESULT_DIR  Directory to store model results in [default: results/]"

  dat <- docopt::docopt(usage, args)
  list(port = as.integer(dat$port),
       queue_id = dat$queue_id,
       workers = as.integer(dat$workers),
       results_dir = dat$results_dir)
}

main_api <- function(args = commandArgs(TRUE)) {
  dat <- main_api_args(args) # nocov
  api(dat$port, dat$queue_id, dat$workers, results_dir) # nocov
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
