Queue <- R6::R6Class(
  "Queue",
  cloneable = FALSE,
  public = list(
    root = NULL,
    stop_workers_on_exit = NULL,
    delete_data_on_exit = FALSE,
    controller = NULL,
    worker_ids = NULL,
    results_dir = NULL,
    inputs_dir = NULL,

    health_check_interval = NULL,
    next_health_check = NULL,

    initialize = function(queue_id = NULL, workers = 0,
                          stop_workers_on_exit = workers > 0,
                          delete_data_on_exit = FALSE,
                          results_dir = tempdir(),
                          inputs_dir = NULL,
                          timeout = Inf,
                          health_check_interval = 0) {
      self$stop_workers_on_exit <- stop_workers_on_exit
      self$delete_data_on_exit <- delete_data_on_exit
      self$results_dir <- results_dir

      message(t_("QUEUE_CONNECTING", list(redis = redux::redis_config()$url)))
      con <- hiredis()

      message(t_("QUEUE_STARTING"))
      queue_id <- hintr_queue_id(queue_id)
      self$controller <- rrq::rrq_controller(queue_id, con = con)
      register_workers(self$controller)
      self$start(workers, timeout)

      message(t_("QUEUE_CACHE"))
      set_cache(queue_id)

      self$inputs_dir <- inputs_dir

      self$health_check_interval <- health_check_interval
      if (health_check_interval > 0) {
        self$next_health_check <- Sys.time() + health_check_interval
      }
    },

    # On cloud services tcp connections are closed after sitting idle.
    # And using tcp_keepalive doesn't work. The client itself needs to
    # reconnect if this has dropped. See
    # https://learn.microsoft.com/en-us/azure/azure-cache-for-redis/cache-best-practices-connection#idle-timeout
    # Would be nice if we could do this in a background process when we form the
    # but it's hard to share a connection between processes, and indeed callr
    # cannot serialize the redis connection.
    health_check = function() {
      if (!is.null(self$next_health_check) &&
          Sys.time() > self$next_health_check) {
        self$controller$con$reconnect()
      }
    },

    start = function(workers, timeout) {
      if (workers > 0L) {
        worker_manager <- rrq::rrq_worker_spawn(
          workers, controller = self$controller)
        self$worker_ids <- worker_manager$id
        if (is.finite(timeout) && timeout > 0) {
          rrq::rrq_message_send_and_wait("TIMEOUT_SET", timeout,
                                         self$worker_ids,
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

    submit_wake = function(queue_name) {
      rrq::rrq_task_create_expr(
        1 + 1,
        queue = queue,
        separate_process = FALSE,
        controller = self$controller
      )
    },

    submit_model_run = function(data, options, iso3) {
      results_dir <- self$results_dir
      language <- traduire::translator()$language()
      extra_memory <- isTRUE(options$extra_memory == "true")
      queue_name <- get_queue_from_job_name("fit", iso3, extra_memory)
      rrq::rrq_task_create_expr(
        hintr:::run_model(data, options, results_dir, language),
        queue = queue_name,
        separate_process = TRUE,
        controller = self$controller
      )
    },

    submit_calibrate = function(model_output, calibration_options, iso3) {
      results_dir <- self$results_dir
      language <- traduire::translator()$language()
      rrq::rrq_task_create_expr(
        hintr:::run_calibrate(model_output, calibration_options, results_dir,
                              language),
        queue = get_queue_from_job_name("calibrate", iso3),
        separate_process = TRUE,
        controller = self$controller
      )
    },

    submit_download = function(model_output, type, input, iso3) {
      results_dir <- self$results_dir
      language <- traduire::translator()$language()
      rrq::rrq_task_create_expr(
        hintr:::download(model_output, type, results_dir, input, language),
        queue = get_queue_from_job_name(type, iso3),
        separate_process = TRUE,
        controller = self$controller
      )
    },

    submit_rehydrate = function(output_zip) {
      rrq::rrq_task_create_expr(
        hintr:::rehydrate(output_zip),
        queue = get_queue_from_job_name("rehydrate"),
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

    exists = function(id) {
      rrq::rrq_task_exists(id, controller = self$controller)
    },

    ## Not part of the api exposed functions, used in tests
    remove = function(id) {
      rrq::rrq_task_delete(id, controller = self$controller)
    },

    ## Not part of the api exposed functions, used in tests
    destroy = function() {
      message(sprintf(
        "Deleting all redis data for queue '%s'",
        self$controller$queue_id))
      rrq::rrq_destroy(delete = TRUE, controller = self$controller)
    },

    cleanup = function() {
      if (!is.null(self$controller)) {
        clear_cache(self$controller$queue_id)
        if (self$delete_data_on_exit) {
          message("Stopping all workers")
          rrq::rrq_worker_detect_exited(controller = self$controller)
          worker_stop(type = "kill", controller = self$controller)
          self$destroy()
          self$controller <- NULL
        } else if (self$stop_workers_on_exit && !is.null(self$worker_ids)) {
          message(t_("QUEUE_STOPPING_WORKERS"))
          worker_stop(self$worker_ids, type = "kill",
                      controller = self$controller)
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

# Separate function only for mocking in tests
hiredis <- function() {
  redux::hiredis()
}
