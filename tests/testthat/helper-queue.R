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
  rrq:::rrq_worker$new(queue_id, con,
                       key_alive = NULL,
                       worker_name = worker_name,
                       queue = c(QUEUE_CALIBRATE, QUEUE_RUN),
                       time_poll = 1,
                       timeout = 300,
                       heartbeat_period = 3,
                       verbose = TRUE)
}

test_queue_result <- function(model = mock_model, calibrate = mock_calibrate,
                              clone_output = TRUE, workers = 1,
                              uploads_dir = NULL) {
  queue <- Queue$new(workers = workers, timeout = 300,
                     uploads_dir = uploads_dir)
  withr::defer_parent({
    message("cleaning up workers")
    queue$cleanup()
  })
  if (clone_output) {
    if (is.null(model$version)) {
      model <- clone_old_model_output(model)
      calibrate <- clone_old_model_output(calibrate)
    } else {
      model <- clone_model_output(model)
      calibrate <- clone_model_output(calibrate)
      if (model$version == packageVersion("naomi")) {
        calibrate$warnings <- list(
          list(
            text = "ART coverage greater than 100% for 10 age groups",
            locations = "model_calibrate"
          ),
          list(
            text = "Prevalence greater than 40%",
            locations = c("model_calibrate", "review_output")
          )
        )
      }
    }
  }
  model_run_id <- queue$submit(quote(identity(model)))
  calibrate_id <- queue$submit(quote(identity(calibrate)))
  queue$queue$task_wait(model_run_id)
  queue$queue$task_wait(calibrate_id)
  list(
    queue = queue,
    model_run_id = model_run_id,
    calibrate_id = calibrate_id
  )
}

build_test_rehydrate <- function(calibrate_output = mock_calibrate) {
  rehydrate_zip <- naomi::save_rehydrate_zip(calibrate_output,
                                             hash_filenames = FALSE)

  ## Copy input files into place
  t <- tempfile()
  dir.create(t)
  data <- c("testdata/Malawi2019.PJNZ", "testdata/population.csv",
            "testdata/malawi.geojson", "testdata/programme.csv",
            "testdata/anc.csv", "testdata/survey.csv")
  # dest_names <- paste0(toupper(tools::md5sum(data)), ".", tools::file_ext(data))
  file.copy(data, t)

  list(
    uploads_dir = t,
    rehydrate_zip = rehydrate_zip
  )
}

response_from_json <- function(x) {
  jsonlite::parse_json(httr::content(x, "text", encoding = "UTF-8"))
}
