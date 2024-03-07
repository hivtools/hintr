adr_metadata <- function(queue) {
  function(id) {
    tryCatch({
      res <- queue$result(id)
      if (is_error(res)) {
        msg <- api_error_msg(res)
        hintr_error(msg, "OUTPUT_GENERATION_FAILED")
      } else if (is.null(res$path)) {
        msg <- t_("FAILED_ADR_METADATA")
        hintr_error(msg, "OUTPUT_GENERATION_FAILED")
      }
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
