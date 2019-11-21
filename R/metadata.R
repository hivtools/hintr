do_plotting_metadata <- function(iso3) {
  metadata <- naomi::get_plotting_metadata(iso3)
  lapply(split(metadata, metadata$data_type), build_data_type_metadata)
}

build_data_type_metadata <- function(metadata) {
  lapply(split(metadata, metadata$plot_type), build_plot_type_metadata)
}

build_plot_type_metadata <- function(metadata) {
  list(indicators =
         lapply(metadata$indicator, function(indicator) {
           build_indicator_metadata(metadata[metadata$indicator == indicator, ])
           }))
}

build_indicator_metadata <- function(metadata) {
  if (nrow(metadata) != 1) {
    stop("Expected only 1 row for indicator, data type, plot type combination.
Check each combination is unique in configuration.")
  }
  list(
    indicator = scalar(metadata$indicator),
    value_column = scalar(metadata$value_column),
    indicator_column = scalar(metadata$indicator_column),
    indicator_value = scalar(metadata$indicator_value),
    name = scalar(metadata$name),
    min = scalar(metadata$min),
    max = scalar(metadata$max),
    colour = scalar(metadata$colour),
    invert_scale = scalar(metadata$invert_scale)
  )
}

get_barchart_metadata <- function(output) {
  metadata <- naomi::get_metadata()
  metadata[
    metadata$data_type == "output" & metadata$plot_type == "barchart",
    c("indicator", "value_column", "error_low_column", "error_high_column",
      "indicator_column", "indicator_value", "name")]
}

get_choropleth_metadata <- function(output) {
  iso3 <- get_root_node(output$area_id)
  metadata <- naomi::get_plotting_metadata(iso3)
  metadata[
    metadata$data_type == "output" & metadata$plot_type == "choropleth",
    c("indicator", "value_column", "indicator_column", "indicator_value",
      "name", "min", "max", "colour", "invert_scale")]
}

get_root_node <- function(area_ids) {
  unique(area_ids[!grepl("\\_", area_ids)])
}
