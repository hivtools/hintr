is_current_version <- function(version) {
  check_version <- function(component) {
    cfg$version_info[[component]] == version[[component]]
  }
  matching_versions <- vapply(names(version), check_version, logical(1))
  all(matching_versions) && length(version) == length(cfg$version_info)
}
