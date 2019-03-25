
column_status <- function(x) {
  structure(x, class = unique(c("rhub_column_status", class(x))))
}

#' @export

`[.rhub_column_status` <- function(x, i) {
  column_status(NextMethod("["))
}

#' @importFrom pillar pillar_shaft new_pillar_shaft_simple
#' @export

pillar_shaft.rhub_column_status <- function(x, ...) {
  ## status can be
  ## - created
  ## - in-progress
  ## - parseerror (the R-hub output parser failed)
  ## - preperror (build failed before R CMD check has started
  ## - aborted (build was aborted)
  ## - error
  ## - warning
  ## - note
  ## - ok

  hst <- c(
    "created"     = status_style_created("created"),
    "in-progress" = status_style_in_progress("in-progress"),
    "parseerror"  = status_style_error("parseerror"),
    "preperror"   = status_style_error("preperror"),
    "aborted"     = status_style_aborted("aborted"),
    "error"       = status_style_error("error"),
    "warning"     = status_style_error("warning"),
    "note"        = status_style_note("note"),
    "ok"          = status_style_ok("ok"))

  cx <- hst[x]
  cx[is.na(cx)] <- x[is.na(cx)]

  new_pillar_shaft_simple(cx, ...)
}

#' @importFrom pillar type_sum
#' @export

type_sum.rhub_column_status <- function(x) {
  "rhub::status"
}
