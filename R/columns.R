get_programme_time_series_columns <- function(data) {
  list(
    list(
      id = scalar("plot_type"),
      column_id = scalar("plot"),
      label = scalar(t_("INPUT_TIME_SERIES_COLUMN_PLOT_TYPE")),
      values = recursive_scalar(
        naomi::get_plot_type_column_metadata(unique(data$plot)))
    ),
    list(
      id = scalar("area_level"),
      column_id = scalar("area_level"),
      label = scalar(t_("INPUT_TIME_SERIES_COLUMN_AREA_LEVEL")),
      values = get_default_id_label_map(data, "area_level", "area_level_label")
    ),
    list(
      id = scalar("time_period"),
      column_id = scalar("time_period"),
      label = scalar(t_("INPUT_TIME_SERIES_COLUMN_TIME_PERIOD")),
      values = get_default_id_label_map(data, "time_period")
    ),
    list(
      id = scalar("quarter"),
      column_id = scalar("quarter"),
      label = scalar(t_("INPUT_TIME_SERIES_COLUMN_QUARTER")),
      values = get_quarter_id_label_map(data[["quarter"]])
    ),
    list(
      id = scalar("area"),
      column_id = scalar("area_id"),
      label = scalar(t_("INPUT_TIME_SERIES_COLUMN_AREA")),
      values = get_area_hierarchy(data)
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
        naomi::get_plot_type_column_metadata(unique(data$plot)))
    ),
    list(
      id = scalar("area_level"),
      column_id = scalar("area_level"),
      label = scalar(t_("INPUT_TIME_SERIES_COLUMN_AREA_LEVEL")),
      values = get_default_id_label_map(data, "area_level", "area_level_label")
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
      id = scalar("quarter"),
      column_id = scalar("quarter"),
      label = scalar(t_("INPUT_TIME_SERIES_COLUMN_QUARTER")),
      values = get_quarter_id_label_map(data[["quarter"]])
    ),
    list(
      id = scalar("area"),
      column_id = scalar("area_id"),
      label = scalar(t_("INPUT_TIME_SERIES_COLUMN_AREA")),
      values = get_area_hierarchy(data)
    ),
    list(
      id = scalar("area_name"),
      column_id = scalar("area_name"),
      label = scalar(t_("INPUT_TIME_SERIES_COLUMN_AREA_NAME")),
      values = get_default_id_label_map(data, "area_name")
    )
  )
}

get_default_id_label_map <- function(data, id_column, label_column = NULL,
                                     capitalise = FALSE) {
  if (is.null(label_column)) {
    label_column <- id_column
  }
  values <- unique(data[, c(id_column, label_column)])
  if (length(unique(data[, id_column])) != nrow(values)) {
    stop(t_("INVALID_ID_LABEL",
            list(id_col = id_column, label_col = label_column)))
  }
  map_single_value <- function(id, label) {
    if (capitalise) {
      label <- to_upper_first(label)
    }
    list(
      id = scalar(id),
      label = scalar(label)
    )
  }
  Map(map_single_value, as.character(values[, id_column]),
      as.character(values[, label_column]), USE.NAMES = FALSE)
}

get_area_hierarchy <- function(data) {
  hierarchy_table <- unique(
    data[, c("area_id", "parent_area_id", "area_sort_order", "area_name")])
  colnames(hierarchy_table) <- c("id", "parent_id", "sort_order", "label")
  construct_tree(hierarchy_table)
}

get_quarter_id_label_map <- function(quarters) {
  quarters <- unique(quarters)
  quarter_map <- list(
    list(
      id = scalar("Q1"),
      label = scalar(t_("QUARTER_1"))
    ),
    list(
      id = scalar("Q2"),
      label = scalar(t_("QUARTER_2"))
    ),
    list(
      id = scalar("Q3"),
      label = scalar(t_("QUARTER_3"))
    ),
    list(
      id = scalar("Q4"),
      label = scalar(t_("QUARTER_4"))
    )
  )
  keep <- vlapply(quarter_map, function(quarter) {
    quarter$id %in% quarters
  })
  quarter_map[keep]
}
