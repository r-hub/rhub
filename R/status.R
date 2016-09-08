
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
}

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
    for (l in lines) fun(l)
  }
}

#' @export

check_status <- function(id, interactive = interactive()) {
  if (interactive) {
    my_curl_stream(id$`log-url`, byline(function(x) cat(x, "\n", sep = "")))
  }
  id
}
