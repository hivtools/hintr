## Takes list of files e.g.
## list(spectrum = list(path = "path/to/file", filename = "...", ...),
##      summary = list(path = "another/path", filename = "...", ...))
## and returns e.g.
## list(spectrum = list(path = "path/to/file", seaweed_id = "1,c434c75216",
##                  filename = "...", ...),
##      summary = list(path = "another/path", seaweed_id = "8,07418f5b8e",
##                   filename = "...", ...))
kelp_save_files <- function(kelp, files) {
  lapply(files, function(file) {
    file$seaweed_id <- kelp$upload_file(file$path)
    file
  })
}

## Takes list of paths or other data e.g.
## list(spectrum = "path/to/file",
##      summary = "another/path",
##      additional_data = list(iso3 = "MWI"))
## and returns e.g.
## list(spectrum = "1,c434c75216",
##      summary = "8,07418f5b8e",
##      additional_data = list(iso3 = "MWI"))
kelp_save_paths <- function(kelp, paths) {
  lapply(paths, function(path) {
    if (is_file_path(path)) {
      path <- kelp$upload_file(path)
    }
    path
  })
}

## Takes list of files e.g.
## list(spectrum = list(seaweed_id = "1,c434c75216", filename = "...", ...),
##      summary = list(seaweed_id = "8,07418f5b8e", filename = "...", ...))
## and returns e.g.
## list(spectrum = list(path = "path/to/file", filename = "...", ...),
##      summary = list(path = "another/path", filename = "...", ...))
## Note if input list contains "path" value will be overwritten with new
## downloaded path
kelp_download_files <- function(kelp, files, dir) {
  lapply(files, function(file) {
    file$path <- kelp$download_file(file$seaweed_id,
                                    file.path(dir, file$filename))
    file
  })
}

## Takes list of paths or other data e.g.
## list(spectrum = "1,c434c75216",
##      summary = "8,07418f5b8e",
##      additional_data = list(iso3 = "MWI"))
## downloads files and returns local paths e.g.
## list(spectrum = "path/to/file",
##      summary = "another/path",
##      additional_data = list(iso3 = "MWI"))
kelp_download_paths <- function(kelp, paths, dir) {
  lapply(paths, function(path) {
    if (is_kelp_id(path)) {
      path <- kelp$download_file(path, tempfile(tmpdir = dir))
    }
    path
  })
}

is_kelp_id <- function(obj) {
  is.character(obj) && length(obj) == 1 && grepl("\\d+,[A-Za-z0-9]{10}", obj)
}

is_file_path <- function(obj) {
  is.character(obj) && length(obj) == 1 && file.exists(obj)
}
