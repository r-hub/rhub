
baseurl <- "https://builder.r-hub.io/api"

endpoints <- list(
  c("GET PLATFORMS", "GET", "/platform/list"),
  c("VALIDATE EMAIL", "POST", "/check/validate_email"),
  c("SUBMIT PACKAGE", "POST", "/check/submit")
)

default_headers <- c(
  "Accept"       = "application/json",
  "Content-Type" = "application/json",
  "User-Agent"   = "R-hub client"
)

#' @importFrom httr GET POST DELETE add_headers
#' @importFrom jsonlite toJSON

query <- function(endpoint, data = NULL, headers = character(),
                  as = NULL) {

  headers <- update(default_headers, as.character(headers))
  ep <- get_endpoint(endpoint)

  url <- paste0(baseurl, ep$path)

  json <- if (!is.null(data)) toJSON(data)

  response <- if (ep$method == "GET") {
    GET(url, add_headers(.headers = headers))

  } else if (ep$method == "POST") {
    POST(url, add_headers(.headers = headers), body = json)

  } else if (ep$method == "DELETE") {
    DELETE(url, add_headers(.headers = headers))

  } else {
    stop("Unexpected HTTP verb, internal rhub error")
  }

  report_error(response)

  parse_response(response, as = as)
}

get_endpoint <- function(endpoint) {

  idx <- match(endpoint, vapply(endpoints, "[[", "", 1))
  if (is.na(idx)) stop("Unknown API endpoint: ", sQuote(endpoint))

  list(method = endpoints[[idx]][2], path = endpoints[[idx]][3])
}

#' @importFrom httr headers content
#' @importFrom jsonlite fromJSON

parse_response <- function(response, as = NULL) {

  content_type <- headers(response)$`content-type`

  if (is.null(content_type) || length(content_type) == 0) {
    ""

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
