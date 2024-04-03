adr_metadata <- function(queue) {
  function(id) {
    tryCatch({
      res <- get_download_result(queue, id, "FAILED_ADR_METADATA")
      list(type = scalar(res$metadata$type),
           description = scalar(res$metadata$description))
    },
    error = function(e) {
      if (is_porcelain_error(e)) {
        stop(e)
      } else {
        hintr_error(api_error_msg(e), "FAILED_TO_RETRIEVE_RESULT")
      }
    })
  }
}
