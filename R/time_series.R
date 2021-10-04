get_programme_time_series <- function(programme, shape) {
  data <- naomi::prepare_input_time_series_art(programme$path, shape$path)
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  columns <- get_programme_time_series_columns(data)
  area_level_options <- get_selected_mappings(columns, "area_level",
                                              key = "values")
  list(
    data = data[, c("area_id", "area_name", "area_level", "time_step",
                    "time_period", "plot", "value")],
    metadata = list(
      columns = columns,
      defaults = list(
        selected_filter_options = list(
          plot_type = get_selected_mappings(columns, "plot_type",
                                            key = "values")[1],
          area_level = area_level_options[length(area_level_options)],
          time_step = get_selected_mappings(columns, "time_step",
                                            key = "values")[1]
        )
      )
    )
  )
}

get_anc_time_series <- function(anc, shape) {
  data <- naomi::prepare_input_time_series_anc(anc$path, shape$path)
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  columns <- get_anc_time_series_columns(data)
  area_level_options <- get_selected_mappings(columns, "area_level",
                                              key = "values")
  list(
    data = data[, c("area_id", "area_name", "area_level", "age_group",
                    "time_period", "time_step", "plot", "value")],
    metadata = list(
      columns = columns,
      defaults = list(
        selected_filter_options = list(
          plot_type = get_selected_mappings(columns, "plot_type",
                                            key = "values")[1],
          area_level = area_level_options[length(area_level_options)],
          age = get_selected_mappings(columns, "age", key = "values")[1]
        )
      )
    )
  )
}
