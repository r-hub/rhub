baseurl <- function() {
  paste0(Sys.getenv("RHUB_SERVER", "https://builder2.rhub.io"), "/api/-")
}

default_headers <- c(
  "Accept"       = "application/json",
  "Content-Type" = "application/json",
  "User-Agent"   = "R-hub client"
)

#' @importFrom jsonlite toJSON

query <- function(endpoint, method = "GET", headers = character(),
                  data = NULL) {

  url <- paste0(baseurl(), endpoint)
  headers <- update(default_headers, headers)

  response <- if (method == "GET") {
    synchronise(http_get(url, headers = headers))

  } else if (method == "POST") {
    synchronise(http_post(url, headers = headers, data = data))

  } else {
    stop("Unexpected HTTP verb, internal rhub error")
  }

  http_stop_for_status(response)

  response
}
