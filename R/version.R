is_current_version <- function(version) {
  isTRUE(all.equal(cfg$version_info, version))
}

update_options <- function(options, version) {
  ## Update version to version and add a test that migration between versions
  ## works as expected

  options
}
