make_logger <- function(log_level, path = NULL) {
  logger <- lgr::get_logger("hintr", reset = TRUE)
  logger$set_propagate(FALSE)
  logger$set_threshold(log_level)
  if (is.null(path)) {
    appender <- lgr::AppenderConsole$new(layout = lgr::LayoutJson$new())
  } else {
    appender <- lgr::AppenderJson$new(path)
  }
  logger$add_appender(appender, name = "json")
  logger
}
