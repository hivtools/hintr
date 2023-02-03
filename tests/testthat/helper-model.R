## Use to return mock data from model run instead of always running the model
Sys.setenv("USE_MOCK_MODEL" = "true")

## Create mock model if files exist
mock_model <- list(
  plot_data_path = NULL,
  model_output_path =
    system.file("output", "malawi_model_output.qs", package = "hintr"),
  version = utils::packageVersion("naomi"),
  warnings = list(
    list(
      text = scalar(paste0("Zero population input for 8 population ",
                           "groups. Replaced with population 0.1.")),
      locations = list(scalar("model_fit"))
    )
  )
)
class(mock_model) <- "hintr_output"

mock_calibrate <- list(
  plot_data_path =
    system.file("output", "malawi_calibrate_plot_data.qs", package = "hintr"),
  model_output_path =
    system.file("output", "malawi_calibrate_output.qs", package = "hintr"),
  version = utils::packageVersion("naomi"),
  warnings = list(
    list(
      text = "ART coverage greater than 100% for 10 age groups",
      locations = list("model_calibrate")
    ),
    list(
      text = "Prevalence greater than 40%",
      locations = list("model_calibrate", "review_output")
    )
  )
)
class(mock_calibrate) <- "hintr_output"

mock_spectrum <- list(
  path = system.file("output", "malawi_spectrum_download.zip",
                     package = "hintr"),
  metadata = list(
    areas = "MWI",
    description = "spectrum desc",
    type = "spectrum"
  )
)

mock_coarse_output <- list(
  path = system.file("output", "malawi_coarse_output_download.zip",
                     package = "hintr"),
  metadata = list(
    areas = "MWI",
    type = "coarse_output"
  )
)

mock_summary <- list(
  path = system.file("output", "malawi_summary_report.html", package = "hintr"),
  metadata = list(
    areas = "MWI",
    description = "summary desc",
    type = "summary"
  )
)

test_mock_model_available <- function() {
  mock_data <- c(mock_model$model_output_path, mock_calibrate$plot_data_path,
                 mock_calibrate$model_output_path, mock_spectrum$path,
                 mock_coarse_output$path, mock_summary$path)
  invisible(lapply(mock_data, function(x) {
    if(!is.list(x) && !file.exists(x)) {
      testthat::skip(sprintf(
        "Test data %s is missing - run ./scripts/build_test_data to create test data.", x))
    }
  }))
}

## Model calibrate output as returned by
## hintr version 0.1.39 to 1.0.7 and naomi version 2.4.3 to 2.5.6
mock_calibrate_v1.0.7 <- list(
  plot_data_path =
    system.file("output", "malawi_calibrate_plot_data.qs", package = "hintr"),
  model_output_path =
    system.file("output", "malawi_calibrate_output.qs", package = "hintr"),
  version = "2.5.6"
)
class(mock_calibrate) <- "hintr_output"

## Model output as returned by
## hintr version 0.1.39 to 1.0.7 and naomi version 2.4.3 to 2.5.4
mock_model_v1.0.7 <- list(
  plot_data_path = NULL,
  model_output_path =
    system.file("output", "malawi_model_output.qs", package = "hintr"),
  version = "2.5.4"
)
class(mock_model) <- "hintr_output"

## Model output as returned by
## hintr version 0.1.4 to 0.1.38 and naomi version 1.0.8 to 2.4.2
mock_model_v0.1.38 <- list(
  output_path = system.file("output", "malawi_output.qs", package = "hintr"),
  spectrum_path = system.file("output", "malawi_spectrum_download.zip",
                              package = "hintr"),
  coarse_output_path =
    system.file("output", "malawi_coarse_output_download.zip",
                package = "hintr"),
  summary_report_path =
    system.file("output", "malawi_summary_report.html", package = "hintr"),
  calibration_path = system.file("output", "malawi_calibration.qs",
                                 package = "hintr"))
class(mock_model_v0.1.38) <- "hintr_output"

mock_model_v0.1.2 <- list(
  output_path = system.file("output", "malawi_output.qs", package = "hintr"),
  spectrum_path = system.file("output", "malawi_spectrum_download.zip",
                              package = "hintr"),
  coarse_output_path =
    system.file("output", "malawi_coarse_output_download.zip",
                package = "hintr"))
class(mock_model_v0.1.2) <- "hintr_output"


clone_model_output <- function(output) {
  model_output_path <- tempfile(fileext = ".qs")
  file.copy(output$model_output_path, model_output_path)
  plot_data_path <- NULL
  if (!is.null(output$plot_data_path)) {
    plot_data_path <- tempfile(fileext = ".qs")
    file.copy(output$plot_data_path, plot_data_path)
  }
  out <- list(model_output_path = model_output_path,
              plot_data_path = plot_data_path,
              version = output$version)
  out$warnings <- output$warnings
  class(out) <- "hintr_output"
  out
}

clone_old_model_output <- function(output) {
  output_path <- tempfile()
  file.copy(output$output_path, output_path)
  spectrum_path <- tempfile(fileext = ".zip")
  file.copy(output$spectrum_path, spectrum_path)
  coarse_output_path <- tempfile(fileext = ".zip")
  file.copy(output$coarse_output_path, coarse_output_path)
  if (!is.null(output$calibration_path)) {
    calibration_path <- tempfile(fileext = ".qs")
    file.copy(output$calibration_path, calibration_path)
  } else {
    calibration_path <- NULL
  }
  if (!is.null(output$summary_report_path)) {
    summary_report_path <- tempfile(fileext = ".qs")
    file.copy(output$summary_report_path, summary_report_path)
  } else {
    summary_report_path <- NULL
  }
  out <- list(output_path = output_path,
              spectrum_path = spectrum_path,
              coarse_output_path = coarse_output_path,
              calibration_path = calibration_path,
              summary_report_path = summary_report_path,
              metadata = output$metadata)
  class(out) <- "hintr_output"
  out
}


wait_status <- function(t, obj, timeout = 2, time_poll = 0.05,
                        status = "PENDING") {
  t_stop <- Sys.time() + timeout
  while (Sys.time() < t_stop) {
    if (all(obj$task_status(t) != status)) {
      return()
    }
    message(".")
    Sys.sleep(time_poll)
  }
  stop(sprintf("Did not change status from %s in time", status))
}

