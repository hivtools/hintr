run_model <- function(data, parameters) {
  Sys.sleep(parameters$sleep)
  data <- read_csv(
    system.file("output", "malawi_output.csv", package = "hintr")
  )
  list(data = select_data(data),
       filters = get_model_output_filters(data))
}

select_data <- function(data) {
  columns <- c("area_id", "sex", "age_group_id", "quarter_id", "indicator_id",
               "mode", "mean", "lower", "upper")
  data[, columns]
}
