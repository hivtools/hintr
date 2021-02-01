test_redis_available <- function() {
  available <- redux::redis_available()
  if (!available) {
    testthat::skip("Skipping test as redis is not available")
  }
  invisible(available)
}

Sys.unsetenv("HINTR_QUEUE_ID")

MockQueue <- R6::R6Class(
  "MockQueue",
  inherit = Queue,
  cloneable = FALSE,
  public = list(
    submit = function(job, queue, environment = parent.frame()) {
      self$queue$enqueue_(quote(stop("test error")))
    },

    submit_model = function(data, options) {
      self$queue$enqueue_(quote(stop("test error")))
    },

    submit_calibrate = function(data, options) {
      self$queue$enqueue_(quote(stop("test error")))
    }
  )
)

test_queue <- function(workers = 2) {
  queue <- Queue$new(workers = workers, timeout = 300)
  withr::defer_parent({
    message("cleaning up workers")
    queue$cleanup()
  })
  queue
}

create_blocking_worker <- function(queue_id, worker_name = NULL) {
  ## Set config for a blocking worker
  con <- redux::hiredis()
  rrq:::rrq_worker_$new(con, queue_id,
                  key_alive = NULL,
                  worker_name = worker_name,
                  queue = c(QUEUE_CALIBRATE, QUEUE_RUN),
                  time_poll = 1,
                  timeout = 300,
                  heartbeat_period = 3,
                  verbose = TRUE)
}
