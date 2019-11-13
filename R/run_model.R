run_model <- function(data, options) {
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
    return(list(output_path = system_file("output", "malawi_output.RDS"),
         spectrum_path = system_file("output", "malawi_spectrum_download.zip"),
         summary_path = system_file("output", "malawi_summary_download.zip")))
  }
  naomi::hintr_run_model(data, options)
}

select_data <- function(data) {
  columns <- c("area_id", "sex", "age_group_id", "quarter_id", "indicator_id",
               "mode", "mean", "lower", "upper")
  data[, columns]
}

process_result <- function(model_output) {
  output <- readRDS(model_output$output_path)
  metadata <- list(
    barchart = list(
      indicators = get_barchart_metadata(output),
      filters = get_model_output_filters(output)
    )
  )
  list(data = select_data(output),
       plottingMetadata = metadata)
}

use_mock_model <- function() {
  Sys.getenv("USE_MOCK_MODEL") == "true"
}
