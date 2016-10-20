
baseurl <- "https://builder.r-hub.io/api"

endpoints <- list(
  c("GET PLATFORMS",  "GET",  "/platform/list"),
  c("VALIDATE EMAIL", "POST", "/check/validate_email"),
  c("SUBMIT PACKAGE", "POST", "/check/submit"),
  c("GET STATUS",     "POST", "/status"),
  c("LIST BUILDS",    "POST", "/list"),
  c("LIVE LOG",       "GET",  "/livelog/text/:id")
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

  headers <- update(default_headers, as.character(headers))
  ep <- get_endpoint(endpoint, params)

  url <- paste0(baseurl, ep$path)

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

  list(method = method, path = path)
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
