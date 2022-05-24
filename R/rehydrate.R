rehydrate <- function(output_zip) {
  files <- zip::zip_list(output_zip$path)
  if (!(PROJECT_STATE_PATH %IN% files$filename)) {
    hintr_error(t_("FAILED_ZIP_REHYDRATE_SUBMIT"), "FAILED_ZIP_REHYDRATE_SUBMIT")
  }
  ## TODO: Need to read this as raw json to return (how do we do this? readLines)
  state <- readBin(unz(output_zip$path, PROJECT_STATE_PATH), "raw")
}

rehydrate_submit <- function(queue) {
  function(input) {
    input <- jsonlite::fromJSON(input)
    assert_file_exists(input$file$path)
    queue$submit_rehydrate(input$file)
  }
}

rehydrate_result <- function(queue) {
  function(id) {
    res <- queue$result(id)
    if (is_error(res) || is.null(res$path)) {
      msg <- res$message
      if (is.null(msg)) {
        msg <- t_("FAILED_ZIP_REHYDRATE")
      }
      hintr_error(msg, "PROJECT_REHYDRATE_FAILED")
    }
    queue$result(id)
  }
}
