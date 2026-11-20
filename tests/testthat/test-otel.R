## A queue that answers what the tested endpoints ask, without redis
otel_mock_queue <- function(exists = function(id) TRUE) {
  list(health_check = function() invisible(NULL),
       exists = exists)
}

## porcelain's api$request() cannot send headers, so build the request here
otel_request <- function(api, method, path, headers = list()) {
  req <- new.env(parent = emptyenv())
  req$REQUEST_METHOD <- method
  req$PATH_INFO <- path
  req$QUERY_STRING <- ""
  req[["rook.input"]] <- list(read_lines = function() NULL,
                              read = function() NULL)
  for (name in names(headers)) {
    req[[paste0("HTTP_", toupper(gsub("-", "_", name)))]] <- headers[[name]]
  }
  api$call(req)
}

otel_record <- function(expr) {
  testthat::skip_if_not_installed("otelsdk")
  otelsdk::with_otel_record(expr, what = "traces")$traces
}

otel_only_span <- function(spans) {
  expect_length(spans, 1)
  spans[[1]]
}

## A trace context as hint would send it
trace_id <- "0af7651916cd43dd8448eb211c80319c"
parent_span_id <- "b7ad6b7169203331"
traceparent <- sprintf("00-%s-%s-01", trace_id, parent_span_id)
no_parent <- "0000000000000000"


test_that("a request has a server span with the route and status", {
  api <- api_build(otel_mock_queue())
  spans <- otel_record({
    res <- otel_request(api, "GET", "/task/abc123/exists")
    expect_equal(res$status, 200)
  })

  span <- otel_only_span(spans)
  expect_equal(span$name, "GET /task/<id>/exists")
  expect_equal(span$kind, "server")
  expect_equal(span$status, "unset")
  expect_equal(span$instrumentation_scope$name, "hintr")
  expect_equal(span$parent, no_parent)
  expect_equal(span$attributes$http.request.method, "GET")
  expect_equal(span$attributes$http.route, "/task/<id>/exists")
  expect_equal(span$attributes$http.response.status_code, 200)
  expect_equal(span$attributes$url.scheme, "http")
})

test_that("a span has no path, query, headers or ids", {
  api <- api_build(otel_mock_queue())
  spans <- otel_record({
    otel_request(api, "GET", "/task/abc123/exists",
                 headers = list(traceparent = traceparent,
                                "accept-language" = "fr"))
  })

  span <- otel_only_span(spans)
  expect_setequal(names(span$attributes),
                  c("http.request.method", "http.route",
                    "http.response.status_code", "url.scheme"))
  expect_false(any(grepl("abc123", unlist(span$attributes))))
  expect_false(grepl("abc123", span$name))
})

test_that("a span continues the trace in the traceparent header", {
  api <- api_build(otel_mock_queue())
  spans <- otel_record({
    otel_request(api, "GET", "/hintr/version",
                 headers = list(traceparent = traceparent,
                                tracestate = "hint=1"))
  })

  span <- otel_only_span(spans)
  expect_equal(span$name, "GET /hintr/version")
  expect_equal(span$trace_id, trace_id)
  expect_equal(span$parent, parent_span_id)
  expect_false(span$span_id == parent_span_id)
})

test_that("a request with no traceparent, or a broken one, starts a trace", {
  api <- api_build(otel_mock_queue())
  spans <- otel_record({
    otel_request(api, "GET", "/hintr/version")
    otel_request(api, "GET", "/hintr/version",
                 headers = list(traceparent = "not-a-traceparent"))
  })

  expect_length(spans, 2)
  for (span in spans) {
    expect_equal(span$parent, no_parent)
    expect_false(span$trace_id == trace_id)
  }
  expect_false(spans[[1]]$trace_id == spans[[2]]$trace_id)
})

test_that("a request that matches no endpoint is named by its method only", {
  api <- api_build(otel_mock_queue())
  spans <- otel_record({
    res <- otel_request(api, "GET", "/not/a/route/xyz")
    expect_equal(res$status, 404)
  })

  span <- otel_only_span(spans)
  expect_equal(span$name, "GET")
  expect_null(span$attributes$http.route)
  expect_equal(span$attributes$http.response.status_code, 404)
  ## A 4xx is not a server error
  expect_equal(span$status, "unset")
})

test_that("a server error marks the span as an error", {
  api <- api_build(otel_mock_queue(exists = function(id) stop("redis is down")))
  spans <- otel_record({
    res <- otel_request(api, "GET", "/task/abc123/exists")
    expect_equal(res$status, 500)
  })

  span <- otel_only_span(spans)
  expect_equal(span$status, "error")
  expect_equal(span$attributes$http.response.status_code, 500)
  expect_equal(span$attributes$error.type, "500")
})

test_that("a failing hook still ends the span", {
  queue <- otel_mock_queue()
  queue$health_check <- function() stop("redis is down")
  api <- api_build(queue)
  spans <- otel_record({
    res <- otel_request(api, "GET", "/hintr/version")
    expect_equal(res$status, 500)
  })

  span <- otel_only_span(spans)
  expect_equal(span$status, "error")
  expect_equal(span$attributes$http.response.status_code, 500)
})

test_that("the request that submits a model run has a span", {
  test_redis_available()
  queue <- test_queue()
  api <- api_build(queue)
  payload <- setup_payload_submit()
  spans <- otel_record({
    res <- api$request("POST", "/model/submit", body = payload)
    expect_equal(res$status, 200)
  })

  span <- otel_only_span(spans)
  expect_equal(span$name, "POST /model/submit")
  expect_equal(span$attributes$http.route, "/model/submit")
  expect_equal(span$attributes$http.response.status_code, 200)
})

test_that("with no exporter, tracing is off and the API works", {
  withr::local_envvar(OTEL_TRACES_EXPORTER = NA, OTEL_R_TRACES_EXPORTER = NA)
  expect_false(otel::is_tracing_enabled())

  api <- api_build(otel_mock_queue())
  res <- otel_request(api, "GET", "/hintr/version",
                      headers = list(traceparent = traceparent))
  expect_equal(res$status, 200)
  expect_equal(jsonlite::fromJSON(res$body)$status, "success")
})

test_that("unknown methods are recorded as _OTHER", {
  expect_equal(http_method("GET"), "GET")
  expect_equal(http_method("POST"), "POST")
  expect_equal(http_method("BREW"), "_OTHER")
})
