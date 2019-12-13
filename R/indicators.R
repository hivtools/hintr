#' Filter data to only return rows containing data for the specified indicator
#'
#' This filters data based on metadata. If wide format data it will
#' return the input with no filtering. If long format then it will only
#' return the rows containing the specified indicator.
#'
#' @param data The input data type to filter.
#' @param type The type of input data.
#' @param indicator The indicator to filter on.
#'
#' @return The read data filtered for the indicator
#' @keywords internal
get_indicator_data <- function(file, type, indicator) {
  metadata <- naomi::get_metadata()
  type_metadata <- metadata[
    metadata$data_type == type & metadata$indicator == indicator, ]
  if (nrow(type_metadata) != 1) {
    stop(t_("INDICATORS_ROW_ERROR",
            list(nrow = nrow(type_metadata), type = type,
                 indicator = indicator)))
  }
  data <- read_csv(file$path)
  if (!is.null(type_metadata$indicator_column) && !identical(type_metadata$indicator_column, "")) {
    ## Filter indicator column based on indicator value of data
    ret <- data[data[[type_metadata$indicator_column]] == type_metadata$indicator_value, ]
  } else {
    ret <- data
  }
  ret
}
