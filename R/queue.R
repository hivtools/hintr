Queue <- R6::R6Class(
  "Queue",
  cloneable = FALSE,
  public = list(
    root = NULL,
    cleanup_on_exit = NULL,
    queue = NULL,

    initialize = function(workers = 2, cleanup_on_exit = workers > 0) {
      self$root <- tempfile()
      self$cleanup_on_exit <- cleanup_on_exit
      ctx <- context::context_load(context_init(self$root))

      message("connecting to redis at ", redux::redis_config()$url)
      con <- redux::hiredis()

      message("Starting queue")
      self$queue <- rrq::rrq_controller(ctx, con)

      self$start(workers)
    },

    start = function(workers) {
      if (workers > 0L) {
        rrq::worker_spawn(self$queue, workers)
      }
    },

    submit = function(data, parameters) {
      self$queue$enqueue_(quote(hintr:::run_model(data, parameters)))
    },

    status = function(id) {
      status <- unname(self$queue$task_status(id))
      done <- c("ERROR", "COMPLETE")
      if (status %in% done) {
        list(done = TRUE,
             status = status,
             success = status == "COMPLETE",
             queue = 0)
      } else {
        list(done = FALSE,
             status = status,
             success = NULL,
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

## Support for queue building
context_init <- function(root, name = "hintr") {
  context::context_save(root,
                        sources = character(0),
                        packages = "hintr",
                        name = name)
}
