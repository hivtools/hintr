Queue <- R6::R6Class(
  "Queue",
  cloneable = FALSE,
  public = list(
    root = NULL,
    cleanup_on_exit = NULL,
    queue = NULL,
    results_dir = NULL,
    inputs_dir = NULL,

    initialize = function(queue_id = NULL, workers = 2,
                          cleanup_on_exit = workers > 0,
                          results_dir = tempdir(),
                          inputs_dir = NULL,
                          timeout = Inf) {
      self$cleanup_on_exit <- cleanup_on_exit
      self$results_dir <- results_dir

      message(t_("QUEUE_CONNECTING", list(redis = redux::redis_config()$url)))
      con <- redux::hiredis()

      message(t_("QUEUE_STARTING"))
      queue_id <- hintr_queue_id(queue_id)
      self$queue <- rrq::rrq_controller$new(queue_id, con)
      default_worker_cfg <- rrq::rrq_worker_config(
        heartbeat_period = 10,
        queue = c(QUEUE_CALIBRATE, QUEUE_RUN))
      calibrate_worker_cfg <- rrq::rrq_worker_config(
        heartbeat_period = 10,
        queue = QUEUE_CALIBRATE)
      self$queue$worker_config_save("localhost", default_worker_cfg)
      self$queue$worker_config_save("calibrate_only", calibrate_worker_cfg)

      self$start(workers, timeout)

      message(t_("QUEUE_CACHE"))
      set_cache(queue_id)

      self$inputs_dir <- inputs_dir
    },

    start = function(workers, timeout) {
      if (workers > 0L) {
        worker_manager <- rrq::rrq_worker_spawn(self$queue, workers)
        if (is.finite(timeout) && timeout > 0) {
          self$queue$message_send_and_wait("TIMEOUT_SET", timeout,
                                           worker_manager$id)
        }
      }
    },

    submit = function(job, queue = NULL, environment = parent.frame()) {
      self$queue$enqueue_(job, environment, queue = queue,
                          separate_process = TRUE)
    },

    submit_model_run = function(data, options) {
      results_dir <- self$results_dir
      language <- traduire::translator()$language()
      self$submit(quote(
        hintr:::run_model(data, options, results_dir, language)),
        queue = QUEUE_RUN)
    },

    submit_calibrate = function(model_output, calibration_options) {
      results_dir <- self$results_dir
      language <- traduire::translator()$language()
      self$submit(quote(
        hintr:::run_calibrate(model_output, calibration_options, results_dir,
                              language)),
        queue = QUEUE_CALIBRATE)
    },

    submit_download = function(model_output, type, notes, state) {
      results_dir <- self$results_dir
      language <- traduire::translator()$language()
      self$submit(quote(
        hintr:::download(model_output, type, results_dir, notes, state,
                         language)),
        queue = QUEUE_CALIBRATE)
    },

    submit_rehydrate = function(output_zip) {
      self$submit(quote(hintr:::rehydrate(output_zip)), queue = QUEUE_CALIBRATE)
    },

    status = function(id) {
      status <- unname(self$queue$task_status(id))
      done <- c("ERROR", "DIED", "CANCELLED", "TIMEOUT", "COMPLETE")
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
      clear_cache(r6_private(self$queue)$keys$queue_id)
      if (self$cleanup_on_exit && !is.null(self$queue$con)) {
        message(t_("QUEUE_STOPPING_WORKERS"))
        self$queue$worker_stop(type = "kill")
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
