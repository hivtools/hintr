is_current_version <- function(version) {
  isTRUE(all.equal(cfg$version_info, version))
}
