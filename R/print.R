
#' @importFrom crayon yellow bold

header_line <- function(x, newline = TRUE, endnewline = TRUE) {
  cat(
    if (newline) "\n",
    bold(yellow(x)),
    if (endnewline) "\n",
    sep = ""
  )
}
