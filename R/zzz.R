# We load something from each package to enforce that the package
# version requirements are met
#' @importFrom geojsonio geojson_read
#' @importFrom jsonlite unbox
#' @importFrom naomi get_metadata
#' @importFrom rrq rrq_controller
#' @importFrom traduire t_
NULL


cfg <- new.env(parent = emptyenv())
.onLoad <- function(...) {
  cfg$version_info <- get_version_info() # nocov
  hintr_init_traduire() # nocov
}

get_version_info <- function() {
  packages <- c("hintr", "naomi", "rrq", "traduire")
  value <- lapply(packages, function(p)
    scalar(as.character(utils::packageVersion(p))))
  names(value) <- packages
  value
}

hintr_init_traduire <- function() {
  root <- system.file("traduire", package = "hintr", mustWork = TRUE)
  pattern <- sprintf("%s/{language}-{namespace}.json", root)
  languages <- c("en", "fr")
  namespaces <- "translation"
  traduire::translator_register(NULL, languages[[1]], namespaces[[1]],
                                resource_pattern = pattern,
                                namespaces = namespaces,
                                languages = languages,
                                fallback = "en")
}
