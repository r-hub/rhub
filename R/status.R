
#' Query the status of an r-hub check
#'
#' @param id The check id, an r-hub status URL, or the object retured
#'   by [check()].
#' @return A list with the status of the check. It has entries:
#'   `status`, `submitted` and `duration`. Currently the duration is
#'   only filled when the build has finished.
#'
#' @export

status <- function(id = NULL) {

  id <- id %||% package_data$last_handle

  if (is.null(id)) stop("Could not find a n rhub handle")

  real_id <- if (is.list(id) && !is.null(id$id) && is_string(id$id)) {
    id$id
  } else if (is_string(id)) {
    sub("^.*/([^/]+)$", "\\1", id, perl = TRUE)
  } else {
    stop("Invalid r-hub build id")
  }

  res <- structure(
    query("GET STATUS", params = list(id = real_id)),
    class = "rhub_status"
  )

  res
}

check_status <- function(id, interactive = interactive()) {
  if (interactive) {
    my_curl_stream(id$`log-url`, byline(make_status_parser(id)))
    invisible(id)
  } else {
    id
  }
}

#' @importFrom curl curl

my_curl_stream <- function(url, callback, bufsize = 80) {
  con <- curl(url)
  if(!isOpen(con)) {
    open(con, "rb")
    on.exit(close(con))
  }
  while (length(buf <- readBin(con, raw(), bufsize))) {
    callback(buf)
    Sys.sleep(1)
  }
  cat("\r                                                 \r")
}

#' @importFrom utils tail

byline <- function(fun) {
  buffer <- raw(0)
  function(r) {

    ## Append new chunk to our buffer
    r <- c(buffer, r)
    buffer <- raw(0)

    ## Search for the last newline, if any
    nl <- tail(which(r == charToRaw('\n')), 1)
    if (length(nl) == 0) {
      buffer <<- r
      return()

    } else if (nl != length(r)) {
      buffer <<- r[(nl + 1):length(r)]
      r <- r[1:nl]
    }

    ## Time to convert to string, split into lines, and serve it
    str <- rawToChar(r)
    lines <- strsplit(str, "\n")[[1]]
    Encoding(lines) <- "UTF-8"
    for (l in lines) fun(l)
  }
}

#' @importFrom rcmdcheck rcmdcheck

make_status_parser <- function(id) {

  first <- TRUE
  checking <- FALSE
  formatter <- ("rcmdcheck" %:::% "check_callback")(top_line = FALSE)
  spinner <- c("-", "\\", "|", "/")

  spin <- function() {
    cat("\r", spinner[1], sep = "")
    spinner <<- c(spinner[-1], spinner[1])
  }

  function(x) {

    if (first) {
      header_line("Build started")
      first <<- FALSE
    }

    ## Get rid of potential \r characters
    x <- gsub("[\r]+", "", x)

    ## Checking (already, and still)

    if (checking) {
      if (grepl("^Status: ", x)) {
        checking <<- FALSE
        return(formatter(x))
      } else {
        return(formatter(x))
      }
    }

    ## Not checking (yet, or any more)

    if (grepl("^>>>>>=====+ Running R CMD check", x)) {
      checking <<- TRUE
      x <- sub("^>>>>>=+ ", "", x)
      header_line(x)

    } else if (grepl("^>>>>>=====", x)) {
      x <- sub("^>>>>>=+ ", "", x)
      header_line(x)

    } else if (grepl("^\\+R-HUB-R-HUB-R-HUB", x)) {
      x <- sub("^\\+R-HUB-R-HUB-R-HUB", "", x)
      spin()

    } else {
      spin()
    }
  }
}
