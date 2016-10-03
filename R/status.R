
check_status <- function(id, interactive = interactive()) {
  if (interactive) {
    my_curl_stream(id$`log-url`, byline(make_status_parser(id)))
  }
  invisible(id)
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
