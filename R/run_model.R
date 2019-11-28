run_model <- function(data, options, dir) {
  if (use_mock_model()) {
    progress_start <- list(
      list(
        started = TRUE,
        complete = FALSE,
        name = "Started mock model"
      ),
      list(
        started = FALSE,
        complete = FALSE,
        name = "Finished mock model"
      )
    )

    signalCondition(structure(list(message = progress_start),
                              class = c("progress", "condition")))
    Sys.sleep(5)
    progress_complete <- list(
      list(
        started = TRUE,
        complete = TRUE,
        name = "Started mock model"
      ),
      list(
        started = TRUE,
        complete = FALSE,
        name = "Finished mock model"
      )
    )
    signalCondition(structure(list(message = progress_complete),
                              class = c("progress", "condition")))
    return(list(output_path = system_file("output", "malawi_output.rds"),
         spectrum_path = system_file("output", "malawi_spectrum_download.zip"),
         summary_path = system_file("output", "malawi_summary_download.zip")))
  }
  dir <- normalizePath(dir, mustWork = TRUE)
  output_path <- tempfile(tmpdir = dir, fileext = ".rds")
  spectrum_path <- tempfile(tmpdir = dir, fileext = ".zip")
  summary_path <- tempfile(tmpdir = dir, fileext = ".zip")
  naomi::hintr_run_model(data, options, output_path, spectrum_path,
                         summary_path)
}

select_data <- function(data) {
  columns <- c("area_id", "sex", "age_group", "calendar_quarter",
               "indicator_id", "mode", "mean", "lower", "upper")
  data[, columns]
}

process_result <- function(model_output) {
  output <- readRDS(model_output$output_path)
  output_filters <- get_model_output_filters(output)
  metadata <- list(
    barchart = list(
      indicators = get_barchart_metadata(output),
      filters = output_filters,
      defaults = get_barchart_defaults(output, output_filters)
    ),
    choropleth = list(
      indicators = get_choropleth_metadata(output),
      filters = output_filters
    )
  )
  list(data = select_data(output),
       plottingMetadata = metadata)
}

use_mock_model <- function() {
  Sys.getenv("USE_MOCK_MODEL") == "true"
}
