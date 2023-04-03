## Read json and apply a class so we can use method dispatching for json
hintr_geojson_read <- function(shape, cache = NULL) {
  with_cache(shape$hash, "geojson", cache, {
    json <- geojsonio::geojson_read(shape$path)
    class(json) <- "geojson"
    json
  })
}

read_geojson_data <- function(shape) {
  json <- hintr_geojson_read(shape)
  data <- lapply(json$features, function(x) {
    list(
      area_id = x$properties$area_id,
      area_level = x$properties$area_level
    )
  })
  list_to_data_frame(unique(data))
}

read_csv_regions <- function(csv_file) {
  data <- read_csv(csv_file$path, header = TRUE)
  unique(data$area_id)
}

read_csv <- function(file, ...) {
  ## Make fread error early if any warning thrown e.g. because of
  ## partially read data
  data <- withr::with_options(list(warn = 3),
    data.table::fread(file, ...,
                      blank.lines.skip = TRUE,
                      data.table = FALSE,
                      nThread = 1,
                      na.strings = c("NA", ""))
  )
  data[rowSums(is.na(data)) != ncol(data), ]
}

read_pjnz_iso3 <- function(pjnz) {
  hiv_params <- specio::read_pjn_metadata(pjnz$path)
  iso_numeric_to_alpha_3(hiv_params$iso3)
}

read_pjnz_iso3_from_path <- function(pjnz_path) {
  hiv_params <- specio::read_pjn_metadata(pjnz_path)
  iso_numeric_to_alpha_3(hiv_params$iso3)
}

## Convert numeric iso3 country code to the alpha-3 code
iso_numeric_to_alpha_3 <- function(numeric_iso) {
  spectrum5_countrylist[which(spectrum5_countrylist$Code == numeric_iso),
                        "iso3"]
}

read_geojson_iso3 <- function(shape) {
  json <- hintr_geojson_read(shape)
  ## At this point we have validated there is only data for 1 country so we can
  ## just take the first.
  get_geojson_iso3(json)
}

get_geojson_iso3 <- function(json) {
  substr(json$features[[1]]$properties$area_id, 1, 3)
}

read_geojson_spectrum_region_codes <- function(shape) {
  json <- hintr_geojson_read(shape)
  region_codes <- lapply(json$features, function(x) {
    x$properties$spectrum_region_code
  })
  unique(unlist(region_codes))
}

read_country <- function(pjnz_path) {
  hiv_params <- specio::read_pjn_metadata(pjnz_path)
  hiv_params$country
}
