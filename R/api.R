
baseurl <- function() {
  paste0(Sys.getenv("RHUB_SERVER", "https://builder.r-hub.io"), "/api")
}

rhub_server <- baseurl

endpoints <- list(
  c("GET PLATFORMS",       "GET",  "/platform/list",        FALSE),
  c("VALIDATE EMAIL",      "POST", "/check/validate_email", FALSE),
  c("SUBMIT PACKAGE",      "POST", "/check/submit",         FALSE),
  c("GET STATUS",          "POST", "/status",               FALSE),
  c("GET GROUP STATUS",    "GET",  "/status/group/:id",     FALSE),
  c("LIST BUILDS EMAIL",   "GET",  "/list/:email",          TRUE),
  c("LIST BUILDS PACKAGE", "GET",  "/list/:email/:package", TRUE),
  c("LIVE LOG",            "GET",  "/livelog/text/:id",     FALSE)
)

default_headers <- c(
  "Accept"       = "application/json",
  "Content-Type" = "application/json",
  "User-Agent"   = "R-hub client"
)

#' @importFrom httr GET POST DELETE add_headers
#' @importFrom jsonlite toJSON

query <- function(endpoint, params = list(), data = NULL,
                  query = list(), headers = character(), as = NULL) {

  ep <- get_endpoint(endpoint, params)
  headers <- update(
    update(default_headers, ep$headers),
    as.character(headers))

  url <- paste0(baseurl(), ep$path)

  json <- if (!is.null(data)) toJSON(data)

  response <- if (ep$method == "GET") {
    GET(url, add_headers(.headers = headers), query = query)

  } else if (ep$method == "POST") {
    POST(url, add_headers(.headers = headers), body = json, query = query)

  } else if (ep$method == "DELETE") {
    DELETE(url, add_headers(.headers = headers), query = query)

  } else {
    stop("Unexpected HTTP verb, internal rhub error")
  }

  report_error(response)

  parse_response(response, as = as)
}

get_endpoint <- function(endpoint, params) {

  idx <- match(endpoint, vapply(endpoints, "[[", "", 1))
  if (is.na(idx)) stop("Unknown API endpoint: ", sQuote(endpoint))

  method <- endpoints[[idx]][2]
  path <- endpoints[[idx]][3]

  colons <- re_match_all(path, ":[a-zA-Z0-9_]+")$.match[[1]]

  for (col in colons) {
    col1 <- substring(col, 2)
    value <- params[[col1]] %||% stop("Unknown API parameter: ", col)
    path <- gsub(col, value, path, fixed = TRUE)
  }

  headers <- if (endpoints[[idx]][[4]]) {
    if (is.null(params$token)) {
      stop("Cannot find token, email address is not validated?")
    }
    c("Authorization" = paste("token", params$token))
  }

  list(method = method, path = path, headers = headers)
}

#' @importFrom httr headers content
#' @importFrom jsonlite fromJSON

parse_response <- function(response, as = NULL) {

  content_type <- headers(response)$`content-type`

  if (is.null(content_type) || length(content_type) == 0) {
    content(response, as = "text")

  } else if (grepl("^application/json", content_type, ignore.case = TRUE)) {
    if (is.null(as)) {
      fromJSON(content(response, as = "text"), simplifyVector = FALSE)
    } else {
      content(response, as = as)
    }

  } else {
    content(response, as = "text")
  }
}
