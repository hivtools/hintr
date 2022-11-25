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
        model_output_path = file.path(private$path, hash, "model-output.qs"))
      stopifnot(all(file.exists(vcapply(ret, identity))))
      ret
    },

    import = function(path, model_output = "model-output.qs") {
      if (!file.exists(path)) {
        stop(sprintf("Import directory %s does not exist", path))
      }
      model_output <- private$validate_path(model_output, path)
      hash <- hash_info_inputs(read_info_inputs(model_output))
      import <- file.path(private$path, hash)
      if (file.exists(import)) {
        stop("This set of data has been imported already")
      }
      dir.create(import)
      file_copy(model_output, file.path(import, "model-output.qs"))
      invisible(hash)
    }
))

##' Import prerun model results
##'
##' @title Import prerun model results
##' @param prerun Path to the prerun model store
##' @param path Path to your results
##' @param model_output Path, within \code{path} to your model output qs
##' @export
prerun_import <- function(prerun, path, model_output = "model-output.qs") {
  PrerunModelResults$new(prerun)$import(path, model_output)
}

##' Push prerun model results to naomi.dide.ic.ac.uk
##'
##' @title Push prerun model results to naomi.dide.ic.ac.uk
##' @inheritParams prerun_import
##' @export
prerun_push <- function(path, model_output = "model-output.qs") {
  loadNamespace("ssh")
  session <- ssh::ssh_connect("incoming@naomi.dide.ic.ac.uk")
  on.exit(ssh::ssh_disconnect(session))
  id <- ids::random_id(1, 6)
  dest <- sprintf("incoming/%s", id)
  args <- c("./hintr_prerun_import", dest, "--model-output", model_output)
  command <- paste(args, collapse = " ")
  ssh::scp_upload(session, path, dest)
  code <- ssh::ssh_exec_wait(session, command)
  if (code != 0) {
    stop("Error running remote command")
  }
}

read_info_inputs <- function(path) {
  model_output <- naomi::read_hintr_output(path)
  read.table(text = model_output$info$inputs.csv, header = TRUE, sep = ",",
    stringsAsFactors = FALSE)
}


hash_info_inputs <- function(inputs) {
  inputs$role[inputs$role == "art_number"] <- "programme"
  inputs$role[inputs$role == "anc_testing"] <- "anc"
  ## Hashes created by format_data_input i.e. from
  ## hintr_run_model are lowercase without the file extension
  ## hashes coming from web app are uppercase with the
  ## extension from the original file. Remove these before
  ## getting the digest.
  inputs$md5sum <- vcapply(inputs$md5sum, function(x) {
    tolower(strsplit(x, "\\.")[[1]][1])
  }, USE.NAMES = FALSE)
  digest::digest(inputs$md5sum[order(inputs$role)])
}
