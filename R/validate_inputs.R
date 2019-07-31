validate_inputs <- function(pjnz, shape, population, programme, survey, anc) {
  if (!is.null(pjnz)) {
    country <- read_country(pjnz)
  }
  TRUE
}

read_country <- function(pjnz) {
  ## TODO: Add function to specio to just extract metadata from the PJN and
  ## use this here instead to avoid getting unnecessary data. See mrc-388.
  hiv_params <- specio::read_hivproj_param(pjnz)
  hiv_params$country
}
