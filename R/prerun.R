PrerunModelResults <- R6::R6Class(
  "PrerunModelResults",
  private = list(
    path = NULL,
    validate_path = function(path, dir, name = deparse(substitute(path))) {
      if (grepl("/", path, fixed = TRUE)) {
        stop(sprintf("Path for '%s' must be just the filename, no slashes",
                     name))
      }
      path_full <- file.path(dir, path)
      if (!file.exists(path_full)) {
        stop(sprintf("Path '%s' for '%s' does not exist", path, name))
      }
      file.path(dir, path)
    }
  ),

  public = list(
    initialize = function(path) {
      private$path <- path
      dir.create(private$path, FALSE, TRUE)
    },

    exists = function(inputs) {
      hash <- hash_info_inputs(inputs)
      file.exists(file.path(private$path, hash))
    },

    list = function() {
      dir(private$path, pattern = "^[[:xdigit:]]+$")
    },

    get = function(inputs) {
      self$get_by_hash(hash_info_inputs(inputs))
    },

    get_by_hash = function(hash) {
      p <- file.path(private$path, hash)
      ret <- list(
        output_path = file.path(private$path, hash, "output.rds"),
        spectrum_path = file.path(private$path, hash, "spectrum.zip"),
        summary_path = file.path(private$path, hash, "summary.zip"))
      stopifnot(all(file.exists(vcapply(ret, identity))))
      ret
    },

    import = function(path, output = "output.rds",
                      spectrum = "spectrum.zip", summary = "summary.zip") {
      if (!file.exists(path)) {
        stop(sprintf("Import directory %s does not exist", path))
      }
      output <- private$validate_path(output, path)
      spectrum <- private$validate_path(spectrum, path)
      summary <- private$validate_path(summary, path)
      hash <- hash_info_inputs(read_info_inputs(summary))
      import <- file.path(private$path, hash)
      if (file.exists(import)) {
        stop("This set of data has been imported already")
      }
      dir.create(import)
      file_copy(output, file.path(import, "output.rds"))
      file_copy(spectrum, file.path(import, "spectrum.zip"))
      file_copy(summary, file.path(import, "summary.zip"))
      invisible(hash)
    }
  ))


read_info_inputs <- function(path) {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  zip::unzip(path, "info/inputs.csv", exdir = tmp, junkpaths = TRUE)
  path_inputs <- file.path(tmp, "inputs.csv")
  read_csv(path_inputs)
}


hash_info_inputs <- function(inputs) {
  inputs$role[inputs$role == "art_number"] <- "programme"
  inputs$role[inputs$role == "anc_testing"] <- "anc"
  digest::digest(inputs$md5sum[order(inputs$role)])
}
