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
  get_version_info() # nocov
  path <- system.file("traduire/translations.json",
                      package = "hintr", mustWork = TRUE)
  traduire::translator_register(path, "en")
}

get_version_info <- function() {
  packages <- c("hintr", "naomi", "rrq")
  value <- lapply(packages, function(p)
    scalar(as.character(utils::packageVersion(p))))
  names(value) <- packages
  cfg$version_info <- value
}
