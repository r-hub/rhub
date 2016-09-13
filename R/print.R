
#' @importFrom crayon yellow bold

header_line <- function(x) {
  cat("\n", bold(yellow(x)), "\n", sep = "")
}
