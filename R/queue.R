Queue <- R6::R6Class(
  "Queue",
  cloneable = FALSE,
  public = list(
    root = NULL,
    cleanup_on_exit = NULL,
    queue = NULL,
    results_dir = NULL,
    prerun_dir = NULL,

    initialize = function(queue_id = NULL, workers = 2,
                          cleanup_on_exit = workers > 0,
                          results_dir = tempdir(),
                          prerun_dir = NULL) {
      self$cleanup_on_exit <- cleanup_on_exit
      self$results_dir = results_dir

      message(t_("QUEUE_CONNECTING", list(redis = redux::redis_config()$url)))
      con <- redux::hiredis()

      message(t_("QUEUE_STARTING"))
      queue_id <- hintr_queue_id(queue_id)
      self$queue <- rrq::rrq_controller(queue_id, con)
      self$queue$worker_config_save("localhost", heartbeat_period = 3)

      self$start(workers)

      message(t_("QUEUE_CACHE"))
      set_cache(queue_id)

      self$prerun_dir <- prerun_dir
    },

    start = function(workers) {
      if (workers > 0L) {
        rrq::worker_spawn(self$queue, workers)
      }
    },

    submit = function(data, options) {
      results_dir <- self$results_dir
      prerun_dir <- self$prerun_dir
      language <- traduire::translator()$language()
      self$queue$enqueue_(quote(
        hintr:::run_model(data, options, results_dir, prerun_dir, language)))
    },

    status = function(id) {
      status <- unname(self$queue$task_status(id))
      done <- c("ERROR", "ORPHAN", "INTERRUPTED", "COMPLETE")
      incomplete <- c("MISSING")
      progress <- self$queue$task_progress(id)
      if (status %in% done) {
        list(done = TRUE,
             status = status,
             success = status == "COMPLETE",
             queue = 0,
             progress = progress)
      } else if (status %in% incomplete) {
        list(done = json_verbatim("null"),
             status = status,
             success = json_verbatim("null"),
             queue = self$queue$task_position(id),
             progress = progress)
      } else {
        list(done = FALSE,
             status = status,
             success = json_verbatim("null"),
             queue = self$queue$task_position(id),
             progress = progress)
      }
    },

    result = function(id) {
      self$queue$task_result(id)
    },

    cancel = function(id) {
      self$queue$task_cancel(id)
    },

    ## Not part of the api exposed functions, used in tests
    remove = function(id) {
      self$queue$task_delete(id)
    },

    ## Not part of the api exposed functions, used in tests
    destroy = function() {
      self$queue$destroy(delete = TRUE)
    },

    cleanup = function() {
      clear_cache(self$queue$keys$queue_id)
      if (self$cleanup_on_exit && !is.null(self$queue$con)) {
        message(t_("QUEUE_STOPPING_WORKERS"))
        self$queue$worker_stop()
        self$destroy()
      }
    }
  ),

  private = list(
    finalize = function() {
      self$cleanup()
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
      stop(t_("QUEUE_ID_NOT_SET"))
    }
    id <- sprintf("hintr:%s", ids::random_id())
  }
  id
}
