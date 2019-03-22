
#' @importFrom crayon make_style
#' @importFrom cli symbol

header_line <- function(x) {

  greyish <- make_style("darkgrey")

  cat(
    paste0("\r", greyish(symbol$line), "  "),
    greyish(x),
    "\n",
    sep = ""
  )
}

#' @importFrom crayon yellow
#' @importFrom cli symbol

title_line <- function(x) {

  cat(
    sep ="",
    "\n",
    yellow(paste0(symbol$line, symbol$line, " ", x)),
    "\n\n"
  )
}
