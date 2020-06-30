#' Validate string of JSON against specified schema.
#'
#' This check will only be done if environmental variable VALIDATE_JSON_SCHEMAS
#' is set to true. If VALIDATE_JSON_SCHEMAS is not true then this always
#' returns TRUE.
#'
#' @param json The JSON to validate.
#' @param schema Name of the schema to validate against.
#'
#' @return True if JSON adheres to schema.
#' @keywords internal
validate_json_schema <- function(json, schema, query = NULL) {
  if (!validate_schemas()) {
    return(invisible(TRUE))
  }
  valid <- validate(json, schema, query)
  invisible(valid)
}

validate <- function(json, schema, query = NULL) {
  schema <- system_file("schema", paste0(schema, ".schema.json"))
  jsonvalidate::json_validate(json, schema, engine = "ajv",
                              query = query, error = TRUE)
}

validate_schemas <- function() {
  Sys.getenv("VALIDATE_JSON_SCHEMAS") == "true"
}

scalar <- function(val) {
  if (inherits(val, "scalar")) {
    val
  } else {
    jsonlite::unbox(val)
  }
}

get_input_response_schema <- function(type) {
  paste0(to_upper_first(tolower(type)), "ResponseData")
}

to_upper_first <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

json_verbatim <- function(x) {
  class(x) <- "json"
  x
}

to_json <- function(x) {
  jsonlite::toJSON(x, json_verbatim = TRUE, na = "null")
}
