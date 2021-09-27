get_programme_time_series_columns <- function(data) {
  list(
    list(
      id = scalar("plot_type"),
      column_id = scalar("plot"),
      label = scalar(t_("INPUT_TIME_SERIES_COLUMN_PLOT_TYPE")),
      values = recursive_scalar(
        naomi::get_plot_type_label_and_description(unique(data$plot)))
    ),
    list(
      id = scalar("area_level"),
      column_id = scalar("area_level_label"),
      label = scalar(t_("INPUT_TIME_SERIES_COLUMN_AREA_LEVEL")),
      values = get_default_id_label_map(data, "area_level_label")
    ),
    list(
      id = scalar("time_step"),
      column_id = scalar("time_step"),
      label = scalar(t_("INPUT_TIME_SERIES_COLUMN_TIME_STEP")),
      values = get_default_id_label_map(data, "time_step", capitalise = TRUE)
    ),
    list(
      id = scalar("time_period"),
      column_id = scalar("time_period"),
      label = scalar(t_("INPUT_TIME_SERIES_COLUMN_TIME_PERIOD")),
      values = get_default_id_label_map(data, "time_period")
    ),
    list(
      id = scalar("area_name"),
      column_id = scalar("area_name"),
      label = scalar(t_("INPUT_TIME_SERIES_COLUMN_AREA_NAME")),
      values = get_default_id_label_map(data, "area_name")
    )
  )
}

get_anc_time_series_columns <- function(data) {
  list(
    list(
      id = scalar("plot_type"),
      column_id = scalar("plot"),
      label = scalar(t_("INPUT_TIME_SERIES_COLUMN_PLOT_TYPE")),
      values = recursive_scalar(
        naomi::get_plot_type_label_and_description(unique(data$plot)))
    ),
    list(
      id = scalar("area_level"),
      column_id = scalar("area_level_label"),
      label = scalar(t_("INPUT_TIME_SERIES_COLUMN_AREA_LEVEL")),
      values = get_default_id_label_map(data, "area_level_label")
    ),
    list(
      id = scalar("age"),
      column_id = scalar("age_group"),
      label = scalar(t_("INPUT_TIME_SERIES_COLUMN_AGE")),
      values = get_age_filters(data)
    ),
    list(
      id = scalar("time_period"),
      column_id = scalar("time_period"),
      label = scalar(t_("INPUT_TIME_SERIES_COLUMN_TIME_PERIOD")),
      values = get_default_id_label_map(data, "time_period")
    ),
    list(
      id = scalar("area_name"),
      column_id = scalar("area_name"),
      label = scalar(t_("INPUT_TIME_SERIES_COLUMN_AREA_NAME")),
      values = get_default_id_label_map(data, "area_name")
    )
  )
}

get_default_id_label_map <- function(data, column, capitalise = FALSE) {
  values <- unique(data[, column])
  lapply(values, function(value) {
    if (capitalise) {
      value <- to_upper_first(value)
    }
    list(
      id = scalar(value),
      label = scalar(value)
    )
  })
}
