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

recursive_scalar <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  lapply(x, function(item) {
    if (length(item) > 1 || is.list(item)) {
      out <- recursive_scalar(item)
    } else {
      out <- scalar(item)
    }
    out
  })
}

warnings_scalar <- function(warnings) {
  lapply(warnings, function(warning) {
    list(
      text = scalar(warning$text),
      locations = warning$locations
    )
  })
}

get_input_response_schema <- function(type) {
  paste0(to_upper_first(tolower(type)), "ResponseData")
}

to_upper_first <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

## Verbatim JSON is also tagged "scalar" so that yyjsonr writes it as is,
## rather than wrapping it in an array. jsonlite dispatches on "json" first.
json_verbatim <- function(x) {
  class(x) <- c("json", "scalar")
  x
}

## Serialise with yyjsonr (much faster than jsonlite on large responses) while
## keeping the output jsonlite used to give:
##   * scalar() values are unboxed, everything else stays an array
##   * NA, NaN and Inf become null
##   * numbers are rounded to 4 decimal places (jsonlite's default)
##   * NULL is written as {} (yyjsonr would write null)
to_json <- function(x) {
  json <- yyjsonr::write_json_str(prepare_for_yyjson(x), opts = yyjson_opts())
  class(json) <- "json"
  json
}

yyjson_opts <- function() {
  yyjsonr::opts_write_json(json_verbatim = TRUE, digits = 4L)
}

## Prepare an object so that yyjsonr serialises it the same way as jsonlite
prepare_for_yyjson <- function(x) {
  if (is.null(x)) {
    structure(list(), names = character(0))
  } else if (inherits(x, "json")) {
    ## Any JSON, including from jsonlite::toJSON, must be written as is
    class(x) <- union(class(x), "scalar")
    x
  } else if (is.data.frame(x)) {
    ## Only list columns can hold NULL
    is_list_col <- vapply(x, is.list, logical(1))
    x[is_list_col] <- lapply(x[is_list_col], function(col) {
      lapply(col, prepare_for_yyjson)
    })
    x
  } else if (is.list(x)) {
    x[] <- lapply(x, prepare_for_yyjson)
    x
  } else {
    x
  }
}

json_null <- function() {
  json_verbatim("null")
}
