column_result <- function(x) {
  structure(x, class = unique(c("rhub_column_result", class(x))))
}

#' @export

`[.rhub_column_result` <- function(x, i) {
  column_result(NextMethod("["))
}

#' @importFrom pillar pillar_shaft new_pillar_shaft_simple
#' @export

pillar_shaft.rhub_column_result <- function(x, ...) {
  cx <- lapply(x, color_column_result)
  new_pillar_shaft_simple(cx, ...)
}

color_column_result <- function(x) {
  if (is.null(x)) return("in-progress")
  E <- if (n <- length(x$errors)) status_style_error(strrep("E", n))
  W <- if (n <- length(x$warnings)) status_style_error(strrep("W", n))
  N <- if (n <- length(x$notes)) status_style_note(strrep("N", n))

  switch(
    x$status,
    "parseerror" = status_style_error("parseerror"),
    "preperror" = status_style_error("preperror"),
    "aborted" = status_style_aborted("aborted"),
    "ok" = status_style_ok("ok"),
    paste0(E, W, N))
}

#' @importFrom pillar type_sum
#' @export

type_sum.rhub_column_result <- function(x) {
  "rhub::result"
}
