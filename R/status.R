
#' @importFrom curl curl

my_curl_stream <- function(url, callback, bufsize = 80) {
  con <- curl(url)
  if(!isOpen(con)) {
    open(con, "rb")
    on.exit(close(con))
  }
  while (length(buf <- readBin(con, raw(), bufsize))) callback(buf)
}

#' @export

check_status <- function(id, interactive = interactive()) {
  my_curl_stream(id$`log-url`, function(x) cat(rawToChar(x)))
  invisible()
}
