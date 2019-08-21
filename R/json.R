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
validate_json_schema <- function(json, schema) {
  if (!validate_schemas()) {
    return(invisible(TRUE))
  }
  valid <- validate(json, schema)
  if (!valid) {
    stop(sprintf("JSON \n%s\n does not adhere to schema %s.", json, schema))
  }
  invisible(valid)
}

validate <- function(json, schema) {
  schema <- system_file("schema", paste0(schema, ".schema.json"),
                        package = "hintr")
  jsonvalidate::json_validate(json, schema, engine = "ajv")
}

validate_schemas <- function() {
  Sys.getenv("VALIDATE_JSON_SCHEMAS") == "true"
}

scalar <- function(val) {
  jsonlite::unbox(val)
}
