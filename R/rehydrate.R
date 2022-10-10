rehydrate <- function(output_zip) {
  files <- zip::zip_list(output_zip$path)
  if (!(PROJECT_STATE_PATH %in% files$filename)) {
    stop(t_("FAILED_ZIP_REHYDRATE_SUBMIT"))
  }
  con <- unz(output_zip$path, PROJECT_STATE_PATH)
  on.exit(close(con))
  state <- paste0(readLines(con), collapse = "\n")
  state <- json_verbatim(state)
  notes <- NULL
  if (NOTES_PATH %in% files$filename) {
    notes_con <- unz(output_zip$path, NOTES_PATH)
    notes <- paste0(readLines(notes_con), collapse = "\n")
    on.exit(close(notes_con), add = TRUE)
  }
  list(
    notes = scalar(notes),
    state = scalar(state)
  )
}

rehydrate_submit <- function(queue) {
  function(input) {
    tryCatch({
      input <- jsonlite::fromJSON(input)
      assert_file_exists(input$file$path)
      list(id = scalar(queue$submit_rehydrate(input$file)))
    }, error = function(e) {
      hintr_error(e$message, "REHYDRATE_SUBMIT_FAILED")
    })
  }
}

rehydrate_result <- function(queue) {
  function(id) {
    res <- queue$result(id)
    if (is_error(res)) {
      hintr_error(res$message, "PROJECT_REHYDRATE_FAILED")
    }
    res
  }
}
