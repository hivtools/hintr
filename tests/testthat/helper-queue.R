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
    submit_model_run = function(data, options) {
      rrq::rrq_task_create_expr(stop("test error"),
                                controller = self$controller)
    },

    submit_calibrate = function(data, options) {
      rrq::rrq_task_create_expr(stop("test error"),
                                controller = self$controller)
    }
  )
)

test_queue <- function(queue_id = NULL, workers = 0, delete_data_on_exit = TRUE) {
  queue <- Queue$new(queue_id, workers = workers, timeout = 300,
                     delete_data_on_exit = delete_data_on_exit)
  withr::defer_parent({
    message("cleaning up workers")
    queue$cleanup()
  })
  queue
}

create_blocking_worker <- function(controller, worker_name = NULL) {
  ## Set config for a blocking worker
  blocking_worker_cfg <- rrq::rrq_worker_config(
    queue = c(QUEUE_CALIBRATE, QUEUE_RUN),
    poll_queue = 1,
    timeout_idle = 300,
    heartbeat_period = 3,
    verbose = TRUE)
  rrq::rrq_worker_config_save("blocking", blocking_worker_cfg,
                              controller = controller)
  rrq:::rrq_worker$new(controller$queue_id, "blocking",
                       worker_id = worker_name)
}

test_queue_result <- function(model = mock_model,
                              calibrate = mock_calibrate,
                              clone_output = TRUE,
                              inputs_dir = NULL) {
  queue <- Queue$new(workers = 1, timeout = 300,
                     delete_data_on_exit = TRUE,
                     inputs_dir = inputs_dir)
  ## Replace exists function so when testing rehydrate we avoid
  ## validation issues
  unlockBinding("exists", queue)
  queue$exists <- function(id) TRUE
  lockBinding("exists", queue)
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
      if (model$version == utils::packageVersion("naomi")) {
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
  model_run_id <- rrq::rrq_task_create_expr(
    identity(model),
    controller = queue$controller)
  calibrate_id <- rrq::rrq_task_create_expr(
    identity(calibrate),
    controller = queue$controller)
  rrq::rrq_task_wait(model_run_id, controller = queue$controller)
  rrq::rrq_task_wait(calibrate_id, controller = queue$controller)
  list(
    queue = queue,
    model_run_id = model_run_id,
    calibrate_id = calibrate_id
  )
}

prerun_inputs <- list(
  pjnz = "testdata/Malawi2024.PJNZ",
  population = "testdata/population.csv",
  shape = "testdata/malawi.geojson",
  programme = "testdata/programme.csv",
  anc = "testdata/anc.csv",
  survey = "testdata/survey.csv"
)

setup_prerun_queue <- function() {
  inputs_dir <- tempfile()
  dir.create(inputs_dir)
  data <- c("testdata/Malawi2024.PJNZ", "testdata/population.csv",
            "testdata/malawi.geojson", "testdata/programme.csv",
            "testdata/anc.csv", "testdata/survey.csv")
  file.copy(data, inputs_dir)

  output_dir <- tempfile()
  dir.create(output_dir)
  data <- c(mock_model$model_output_path, mock_calibrate$plot_data_path,
            mock_calibrate$model_output_path)
  file.copy(data, output_dir)

  as_file <- function(filename, dir) {
    list(
      path = scalar(normalizePath(file.path(dir, filename), winslash = "/")),
      filename = scalar(filename)
    )
  }

  payload <- list(
    inputs = list(
      pjnz = as_file("Malawi2024.PJNZ", inputs_dir),
      population = as_file("population.csv", inputs_dir),
      shape = as_file("malawi.geojson", inputs_dir),
      survey = as_file("survey.csv", inputs_dir),
      programme = as_file("programme.csv", inputs_dir),
      anc = as_file("anc.csv", inputs_dir)
    ),
    outputs = list(
      fit_model_output = as_file(basename(mock_model$model_output_path),
                                 output_dir),
      calibrate_plot_data = as_file(basename(mock_calibrate$plot_data_path),
                                    output_dir),
      calibrate_model_output = as_file(
        basename(mock_calibrate$model_output_path), output_dir)
    )
  )

  list(
    queue = Queue$new(inputs_dir = inputs_dir,
                      results_dir = output_dir),
    payload = jsonlite::toJSON(payload)
  )
}


response_from_json <- function(x, ...) {
  jsonlite::parse_json(httr::content(x, "text", encoding = "UTF-8"), ...)
}
