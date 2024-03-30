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
                  data = NULL, data_form = NULL, sse = FALSE) {

  url <- paste0(baseurl(), endpoint)
  headers <- update(default_headers, headers)

  response <- if (sse) {
    query_sse(method, url, headers, data, data_form)
  } else {
    query_plain(method, url, headers, data, data_form)
  }

  if (response$status_code >= 400) {
    cnd <- http_error(response)
    tryCatch({
      bdy <- jsonlite::fromJSON(
        rawToChar(response$content),
        simplifyVector = FALSE
      )
    }, error = function(err) { stop(cnd) })
    if ("message" %in% names(bdy)) {
      throw(new_error(bdy[["message"]]), parent = cnd)
    } else {
      stop(cnd)
    }
  }

  response
}

query_sse <- function(method, url, headers, data, data_form) {
  synchronise(
    query_sse_async(method, url, headers, data, data_form)
  )
}

query_sse_async <- function(method, url, headers, data, data_form) {
  if (method == "GET") {
    q <- http_get(url, headers = headers)
  } else if (method == "POST") {
    q <- http_post(
      url,
      headers = headers,
      data = data,
      data_form = data_form
    )
  } else {
    stop("Unexpected HTTP verb, internal rhub error")
  }

  msgs <- list()
  handle_sse <- function(evt) {
    msgs <<- c(msgs, list(evt))
    if (evt[["event"]] == "progress") {
      msg <- jsonlite::fromJSON(evt[["data"]])
      cli::cli_alert(msg, .envir = emptyenv())
    } else if (evt[["event"]] == "result") {
      cli::cli_alert_success("Done.")
    } else if (evt[["event"]] == "error") {
      msg <- jsonlite::fromJSON(evt[["data"]])
      cli::cli_alert_danger(msg, .envir = emptyenv())
      stop("Aborting")
    }
  }

  evs <- sse_events$new(q)
  evs$listen_on("event", handle_sse)

  q$then(function(response) {
    response$sse <- msgs
    response
  })
}

query_plain <- function(method, url, headers, data, data_form) {
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

  response
}