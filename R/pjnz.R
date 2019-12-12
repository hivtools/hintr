get_pjnz_paths <- function(zip) {
  if (!is_pjnz(zip$path)) {
    unzip_dir <- tempfile("pjnz_unzip")
    dir.create(unzip_dir)
    zip::unzip(zip$path, exdir = unzip_dir)
    pjnz_paths <- list.files(unzip_dir, full.names = TRUE)
    are_pjnz <- lapply(pjnz_paths, is_pjnz)
    if (!all(unlist(are_pjnz))) {
      not_pjnz <- list.files(unzip_dir)[!unlist(are_pjnz)]
      stop(t_("PJNZ_INVALID_ZIP", list(not_pjnz = collapse(not_pjnz))))
    }
  } else {
    pjnz_paths <- zip$path
  }
  pjnz_paths
}

is_pjnz <- function(path) {
  tryCatch({
    files <- zip::zip_list(path)
    any(grepl("*.DP", files$filename))
  },
  error = function(e) {
    return(FALSE)
  })
}
