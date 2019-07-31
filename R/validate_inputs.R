validate_inputs <- function(pjnz, shape, population, programme, survey, anc) {
  if (!is.null(pjnz)) {
    country <- read_country(pjnz)
  }
  TRUE
}

read_country <- function(pjnz) {
  hiv_params <- specio::read_hivproj_param(pjnz)
  hiv_params$country
}
