
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

#' @importFrom cli make_ansi_style style_bold style_inverse
#'   col_red col_blue col_green

status_style_created <- function(x) {
  x
}

status_style_in_progress <- function(x) {
  x
}

status_style_error <- function(x) {
  style_inverse(style_bold(col_red(x)))
}

status_style_aborted <- function(x) {
  style_bold(col_blue(x))
}

status_style_note <- function(x) {
  orange <- make_ansi_style("orange")
  style_bold(orange(x))
}

status_style_ok <- function(x) {
  style_inverse(style_bold(col_green(x)))
}
