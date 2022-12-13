upload_input <- function(queue) {
  function(file, filename) {
    md5 <- toupper(digest::digest(file, algo = "md5"))
    dest_path <- file.path(
      queue$uploads_dir,
      paste0(md5, ".", tools::file_ext(filename)))
    if (!file.exists(dest_path)) {
      t <- tempfile()
      writeBin(file, t)
      file.copy(t, dest_path)
    }
    list(
      path = scalar(dest_path),
      filename = scalar(filename),
      hash = scalar(md5)
    )
  }
}
