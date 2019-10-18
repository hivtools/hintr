Queue <- R6::R6Class(
  "Queue",
  cloneable = FALSE,
  public = list(
    root = NULL,
    cleanup_on_exit = NULL,
    queue = NULL,

    initialize = function(queue_id = NULL, workers = 2,
                          cleanup_on_exit = workers > 0) {
      self$cleanup_on_exit <- cleanup_on_exit

      message("connecting to redis at ", redux::redis_config()$url)
      con <- redux::hiredis()

      message("Starting queue")
      self$queue <- rrq::rrq_controller(hintr_queue_id(queue_id), con)

      self$start(workers)
    },

    start = function(workers) {
      if (workers > 0L) {
        rrq::worker_spawn(self$queue, workers)
      }
    },

    submit = function(data, options) {
      self$queue$enqueue_(quote(hintr:::run_model(data, options)))
    },

    status = function(id) {
      status <- unname(self$queue$task_status(id))
      done <- c("ERROR", "COMPLETE")
      incomplete <- c("MISSING")
      if (status %in% done) {
        list(done = TRUE,
             status = status,
             success = status == "COMPLETE",
             queue = 0)
      } else if (status %in% incomplete) {
        list(done = json_verbatim("null"),
             status = status,
             success = json_verbatim("null"),
             queue = self$queue$task_position(id))
      } else {
        list(done = FALSE,
             status = status,
             success = json_verbatim("null"),
             queue = self$queue$task_position(id))
      }
    },

    result = function(id) {
      self$queue$task_result(id)
    },

    remove = function(id) {
      self$queue$task_delete(id)
    },

    ## Not part of the api exposed functions, used in tests
    destroy = function() {
      self$queue$destroy(delete = TRUE)
    },

    finalize = function(queue) {
      if (self$cleanup_on_exit) {
        message("Stopping workers")
        self$queue$worker_stop()
        self$destroy()
      }
    }
  )
)

hintr_queue_id <- function(queue_id, worker = FALSE) {
  if (!is.null(queue_id)) {
    return(queue_id)
  }
  id <- Sys.getenv("HINTR_QUEUE_ID", "")
  if (!nzchar(id)) {
    if (worker) {
      stop("Environment variable 'HINTR_QUEUE_ID' is not set")
    }
    id <- sprintf("hintr:%s", ids::random_id())
  }
  id
}
