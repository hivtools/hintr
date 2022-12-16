upload_file <- function(dir) {
  function(file, filename) {
    md5 <- toupper(digest::digest(file, algo = "md5", serialize = FALSE))
    dest_path <- to_server_path(dir, md5, filename)
    if (!file.exists(dest_path)) {
      tmpdir <- file.path(dir, ".incoming")
      dir.create(tmpdir, FALSE, TRUE)
      t <- tempfile(tmpdir = tmpdir)
      writeBin(file, t)
      file.rename(t, dest_path)
    }
    list(
      path = scalar(dest_path),
      filename = scalar(filename)
    )
  }
}

to_server_path <- function(root_dir, md5, filename) {
  file.path(root_dir, paste0(md5, ".", tools::file_ext(filename)))
}
