baseurl <- function() {
  paste0(Sys.getenv("RHUB_SERVER", "https://builder2.rhub.io"), "/api/-")
}

default_headers <- c(
  "accept"       = "application/json",
  "content-type" = "application/json",
  "user-agent"   = "R-hub client"
)

#' @importFrom jsonlite toJSON

query <- function(endpoint, method = "GET", headers = character(),
                  data = NULL, data_form = NULL) {

  url <- paste0(baseurl(), endpoint)
  headers <- update(default_headers, headers)

  response <- if (method == "GET") {
    synchronise(http_get(url, headers = headers))

  } else if (method == "POST") {
    synchronise(http_post(
      url,
      headers = headers,
      data = data,
      data_form = data_form
    ))

  } else {
    stop("Unexpected HTTP verb, internal rhub error")
  }

  http_stop_for_status(response)

  response
}
