Queue <- R6::R6Class(
  "Queue",
  cloneable = FALSE,
  public = list(
    root = NULL,
    cleanup_on_exit = NULL,
    controller = NULL,
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
      self$controller <- rrq::rrq_controller2(queue_id, con = con)
      default_worker_cfg <- rrq::rrq_worker_config(
        heartbeat_period = 10,
        queue = c(QUEUE_CALIBRATE, QUEUE_RUN))
      calibrate_worker_cfg <- rrq::rrq_worker_config(
        heartbeat_period = 10,
        queue = QUEUE_CALIBRATE)
      rrq::rrq_worker_config_save2("localhost", default_worker_cfg,
                                   controller = self$controller)
      rrq::rrq_worker_config_save2("calibrate_only", calibrate_worker_cfg,
                                   controller = self$controller)

      self$start(workers, timeout)

      message(t_("QUEUE_CACHE"))
      set_cache(queue_id)

      self$inputs_dir <- inputs_dir
    },

    start = function(workers, timeout) {
      if (workers > 0L) {
        worker_manager <- rrq::rrq_worker_spawn2(workers,
                                                 controller = self$controller)
        if (is.finite(timeout) && timeout > 0) {
          rrq::rrq_message_send_and_wait("TIMEOUT_SET", timeout,
                                         worker_manager$id,
                                         controller = self$controller)
        }
      }
    },

    submit = function(job, queue = NULL) {
      stop("Don't call this directly")
    },

    task_wait = function(id) {
      rrq::rrq_task_wait(id, controller = self$controller)
    },

    submit_model_run = function(data, options) {
      results_dir <- self$results_dir
      language <- traduire::translator()$language()
      rrq::rrq_task_create_expr(
        hintr:::run_model(data, options, results_dir, language),
        queue = QUEUE_RUN,
        separate_process = TRUE,
        controller = self$controller
      )
    },

    submit_calibrate = function(model_output, calibration_options) {
      results_dir <- self$results_dir
      language <- traduire::translator()$language()
      rrq::rrq_task_create_expr(
        hintr:::run_calibrate(model_output, calibration_options, results_dir,
                              language),
        queue = QUEUE_CALIBRATE,
        separate_process = TRUE,
        controller = self$controller
      )
    },

    submit_download = function(model_output, type, input) {
      results_dir <- self$results_dir
      language <- traduire::translator()$language()
      rrq::rrq_task_create_expr(
        hintr:::download(model_output, type, results_dir, input, language),
        queue = QUEUE_CALIBRATE,
        separate_process = TRUE,
        controller = self$controller
      )
    },

    submit_rehydrate = function(output_zip) {
      rrq::rrq_task_create_expr(
        hintr:::rehydrate(output_zip),
        queue = QUEUE_CALIBRATE,
        separate_process = TRUE,
        controller = self$controller
      )
    },

    status = function(id) {
      status <- unname(rrq::rrq_task_status(id, controller = self$controller))
      done <- c("ERROR", "DIED", "CANCELLED", "TIMEOUT", "COMPLETE")
      incomplete <- c("MISSING")
      progress <- rrq::rrq_task_progress(id, controller = self$controller)
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
             queue = rrq::rrq_task_position(id, controller = self$controller),
             progress = progress)
      } else {
        list(done = FALSE,
             status = status,
             success = json_verbatim("null"),
             queue = rrq::rrq_task_position(id, controller = self$controller),
             progress = progress)
      }
    },

    result = function(id) {
      rrq::rrq_task_result(id, controller = self$controller)
    },

    cancel = function(id) {
      rrq::rrq_task_cancel(id, controller = self$controller)
    },

    ## Not part of the api exposed functions, used in tests
    remove = function(id) {
      rrq::rrq_task_delete(id, controller = self$controller)
    },

    ## Not part of the api exposed functions, used in tests
    destroy = function() {
      rrq::rrq_destroy(delete = TRUE, controller = self$controller)
    },

    cleanup = function() {
      if (!is.null(self$controller)) {
        clear_cache(self$controller$queue_id)
        if (self$cleanup_on_exit) {
          message(t_("QUEUE_STOPPING_WORKERS"))
          worker_stop(type = "kill", controller = self$controller)
          self$destroy()
          self$controller <- NULL
        }
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
