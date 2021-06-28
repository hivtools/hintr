adr_metadata <- function(queue) {
  function(id) {
    tryCatch({
      res <- queue$result(id)
      if (is_error(res) || is.null(res$path)) {
        hintr_error(res$message, "OUTPUT_GENERATION_FAILED")
      }
      list(type = scalar(res$metadata$type),
           description = scalar(res$metadata$description))
    },
    error = function(e) {
      if (is_porcelain_error(e)) {
        stop(e)
      } else {
        hintr_error(e$message, "FAILED_TO_RETRIEVE_RESULT")
      }
    })
  }
}
