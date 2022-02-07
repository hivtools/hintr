get_pjnz_paths <- function(zip) {
  if (!is_pjnz(zip$path)) {
    unzip_dir <- tempfile("pjnz_unzip")
    dir.create(unzip_dir)
    zip::unzip(zip$path, exdir = unzip_dir)
    pjnz_paths <- list.files(unzip_dir, full.names = TRUE)
    are_pjnz <- lapply(pjnz_paths, is_pjnz)
    pjnz_paths <- pjnz_paths[unlist(are_pjnz)]
    if (length(pjnz_paths) == 0) {
      stop(t_("PJNZ_INVALID_ZIP"))
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
