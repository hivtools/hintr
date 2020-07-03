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
    submit = function(data, options) {
      self$queue$enqueue_(quote(stop("test error")))
    }
  )
)

test_queue <- function(workers = 2) {
  queue <- Queue$new(workers = workers)
  withr::defer_parent({
    message("cleaning up workers")
    queue$cleanup()
  })
  queue
}
