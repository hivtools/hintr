rehydrate <- function(output_zip) {
  files <- zip::zip_list(output_zip$path)
  if (!(PROJECT_STATE_PATH %in% files$filename)) {
    stop(t_("FAILED_ZIP_REHYDRATE_SUBMIT"))
  }
  con <- unz(output_zip$path, PROJECT_STATE_PATH)
  on.exit(close(con))
  state <- paste0(readLines(con), collapse = "\n")
  ## Mark state as JSON so when serialized it seralizes this JSON as is
  class(state) <- "json"
  scalar(state)
}

rehydrate_submit <- function(queue) {
  function(input) {
    input <- jsonlite::fromJSON(input)
    assert_file_exists(input$file$path)
    queue$submit_rehydrate(input$file)
    list(id = scalar(queue$submit_rehydrate(input$file)))
  }
}

rehydrate_result <- function(queue) {
  function(id) {
    res <- queue$result(id)
    if (is_error(res)) {
      msg <- res$message
      if (is.null(msg)) {
        msg <- t_("FAILED_ZIP_REHYDRATE")
      }
      hintr_error(msg, "PROJECT_REHYDRATE_FAILED")
    }
    res
  }
}
