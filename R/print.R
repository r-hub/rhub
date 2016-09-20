
#' @importFrom crayon make_style
#' @importFrom clisymbols symbol

header_line <- function(x) {

  greyish <- make_style("darkgrey")

  cat(
    paste0("\r", greyish(symbol$line), "  "),
    greyish(x),
    "\n",
    sep = ""
  )
}
