run_model <- function(data, options) {
  if (options$use_mock_model) {
    Sys.sleep(5)
    return(list(output_path = system_file("output", "malawi_output.RDS"),
         spectrum_path = system_file("output", "malawi_spectrum_download.zip"),
         summary_path = system_file("output", "malawi_summary_download.zip")))
  }
  naomi::run_model(data, options, tempfile(), tempfile(), tempfile())
}

select_data <- function(data) {
  columns <- c("area_id", "sex", "age_group_id", "quarter_id", "indicator_id",
               "mode", "mean", "lower", "upper")
  data[, columns]
}

process_result <- function(model_output) {
  output <- readRDS(model_output$output_path)
  list(data = select_data(output),
       filters = get_model_output_filters(output))
}
