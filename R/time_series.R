get_programme_time_series <- function(programme, shape) {
  data <- naomi::prepare_input_time_series_art(programme$path, shape$path)
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  filters <- get_programme_time_series_filters(data)
  area_level_options <- get_selected_filter_options(filters, "area_level")
  list(
    data = data,
    metadata = list(
      filters = filters,
      defaults = list(
        selected_filter_options = list(
          plot_type = get_selected_filter_options(filters, "plot_type")[1],
          area_level = area_level_options[length(area_level_options)],
          time_step = get_selected_filter_options(filters, "time_step")[1]
        )
      )
    )
  )
}

get_anc_time_series <- function(anc, shape) {
  data <- naomi::prepare_input_time_series_anc(anc$path, shape$path)
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  filters <- get_anc_time_series_filters(data)
  area_level_options <- get_selected_filter_options(filters, "area_level")
  list(
    data = data,
    metadata = list(
      filters = filters,
      defaults = list(
        selected_filter_options = list(
          plot_type = get_selected_filter_options(filters, "plot_type")[1],
          area_level = area_level_options[length(area_level_options)],
          age = get_selected_filter_options(filters, "age")[1]
        )
      )
    )
  )
}
