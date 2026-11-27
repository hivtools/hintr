## OpenTelemetry tracing of the API.
##
## Each request gets a server span. If the request has a W3C `traceparent`
## header (hint sends one), the span continues that trace. So hintr's span
## sits under the span of the call that hint made.
##
## Tracing is off unless an exporter is chosen with OTEL_TRACES_EXPORTER.
## The exporters are in the otelsdk package, which is in Suggests. When
## tracing is off, a request costs one check and nothing else.
##
## The spans follow the OpenTelemetry HTTP semantic conventions, with less
## in them. They have the method, the route template, the status and the
## scheme. They do not have the path (it has ids and file names), the query
## string, any header, or the bodies.
##
## Model runs and other queued jobs run in rrq workers. The workers are not
## traced. The request that submits a job has its span like any other.

## The name of the tracer (the "instrumentation scope") for hintr's spans.
## otel looks for this name in the package.
otel_tracer_name <- "hintr"

## The API: porcelain, with a span around each request.
##
## The span starts before plumber's first hook and ends after its last one.
## So it covers the hooks, the filters, the endpoint and the serialising,
## and it ends when the response is ready, even if a hook fails. The span
## is the active span while the request runs, so a span made later in the
## request (by hintr, or by a package such as DBI) is its child.
##
## plumber's hooks cannot do this. A span made in a hook is only active
## inside that hook. And if the preroute hook fails (e.g. redis is down),
## the postserialize hook does not run, so the span would never end.
hintr_api <- R6::R6Class(
  "hintr_api",
  inherit = porcelain::porcelain,
  cloneable = FALSE,

  public = list(
    serve = function(req, res) {
      tracer <- otel::get_tracer()
      if (!otel::is_tracing_enabled(tracer)) {
        return(super$serve(req, res))
      }
      span <- api_span_start(req, tracer)
      withr::defer(api_span_end(span, req, res))
      otel::local_active_span(span)
      super$serve(req, res)
    }
  )
)

## The route is not known until plumber has found the endpoint. So the span
## starts with the method as its name, and gets the route when it ends.
api_span_start <- function(req, tracer) {
  method <- http_method(req$REQUEST_METHOD)
  attributes <- list(http.request.method = method,
                     url.scheme = "http")
  if (method == "_OTHER") {
    attributes$http.request.method_original <- req$REQUEST_METHOD
  }
  options <- list(kind = "server",
                  parent = api_span_parent(req))
  otel::start_span(method, attributes = attributes, options = options,
                   tracer = tracer)
}

## The trace context that the client sent. Without a valid `traceparent`,
## the span starts a new trace (NA means "no parent" to otel).
api_span_parent <- function(req) {
  traceparent <- req$HTTP_TRACEPARENT
  if (is.null(traceparent)) {
    return(NA)
  }
  headers <- list(traceparent = traceparent)
  if (!is.null(req$HTTP_TRACESTATE)) {
    headers$tracestate <- req$HTTP_TRACESTATE
  }
  context <- otel::extract_http_context(headers)
  if (!context$is_valid()) {
    return(NA)
  }
  context
}

api_span_end <- function(span, req, res) {
  method <- http_method(req$REQUEST_METHOD)
  ## porcelain sets this to the path template, e.g. "/model/status/<id>".
  ## It is not set for a request that matches no endpoint (a 404).
  route <- req$porcelain_endpoint
  if (!is.null(route)) {
    span$update_name(paste(method, route))
    span$set_attribute("http.route", route)
  }
  status <- as.integer(res$status)
  span$set_attribute("http.response.status_code", status)
  ## For a server span, only a 5xx is an error. A 4xx is the client's fault.
  if (status >= 500L) {
    span$set_attribute("error.type", as.character(status))
    span$end(status_code = "error")
  } else {
    span$end()
  }
}

## The method, or "_OTHER" for one that HTTP does not define. This keeps
## the number of span names small.
http_method <- function(method) {
  known <- c("CONNECT", "DELETE", "GET", "HEAD", "OPTIONS", "PATCH", "POST",
             "PUT", "TRACE")
  if (method %in% known) method else "_OTHER"
}
