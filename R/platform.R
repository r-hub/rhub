
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom crayon green cyan

platforms <- function() {
  json <- query("GET PLATFORMS", as = "text")
  pls <- fromJSON(json, simplifyDataFrame = TRUE)
  pls <- pls[order(pls$name), , drop = FALSE]
  class(pls) <- c("rhub_platforms", pls)
  pls
}

#' @export

print.rhub_platforms <- function(x, ...) {
  res <- paste(cyan(x$name), green(x$description), sep = ":\n  ")
  cat(res, sep = "\n")
  invisible(x)
}
