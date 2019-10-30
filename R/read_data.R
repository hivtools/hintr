## Read json and apply a class so we can use method dispatching for json
hintr_geojson_read <- function(path) {
  json <- geojsonio::geojson_read(path)
  class(json) <- "geojson"
  json
}

read_geojson_regions <- function(geojson_file) {
  json <- hintr_geojson_read(geojson_file)
  vcapply(json$features, function(x) {
    x$properties$area_id
  })
}

read_csv_regions <- function(csv_file) {
  data <- read_csv(csv_file, header = TRUE)
  unique(data$area_id)
}

read_csv <- function(...) {
  utils::read.csv(..., stringsAsFactors = FALSE)
}

read_pjnz_iso3 <- function(pjnz_file) {
  hiv_params <- specio::read_hivproj_param(pjnz_file)
  iso_numeric_to_alpha_3(hiv_params$iso3)
}

## Convert numeric iso3 country code to the alpha-3 code
iso_numeric_to_alpha_3 <- function(numeric_iso) {
  spectrum5_countrylist[which(spectrum5_countrylist$Code == numeric_iso),
                        "iso3"]
}

read_geojson_iso3 <- function(geojson_file) {
  json <- hintr_geojson_read(geojson_file)
  ## At this point we have validated there is only data for 1 country so we can
  ## just take the first.
  substr(json$features[[1]]$properties$area_id, 1, 3)
}

read_country <- function(pjnz) {
  ## TODO: Add function to specio to just extract metadata from the PJN and
  ## use this here instead to avoid getting unnecessary data. See mrc-388.
  hiv_params <- specio::read_hivproj_param(pjnz)
  hiv_params$country
}

