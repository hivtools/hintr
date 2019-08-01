do_validate_baseline <- function(pjnz, shape, population) {
  read_country(pjnz)
}

read_country <- function(pjnz) {
  ## TODO: Add function to specio to just extract metadata from the PJN and
  ## use this here instead to avoid getting unnecessary data. See mrc-388.
  hiv_params <- specio::read_hivproj_param(pjnz)
  hiv_params$country
}
