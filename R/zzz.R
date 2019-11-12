# We load something from each package to enforce that the package
# version requirements are met
#' @importFrom geojsonio geojson_read
#' @importFrom jsonlite unbox
#' @importFrom naomi get_metadata
#' @importFrom rrq rrq_controller
NULL


cfg <- new.env(parent = emptyenv())
.onLoad <- function(...) {
  packages <- c("hintr", "naomi", "rrq")
  value <- lapply(packages, function(p)
    scalar(as.character(utils::packageVersion(p))))
  names(value) <- packages
  cfg$version_info <- value
}
