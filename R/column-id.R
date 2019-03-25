
column_id <- function(x) {
  structure(x, class = unique(c("rhub_column_id", class(x))))
}

#' @export

`[.rhub_column_id` <- function(x, i) {
  column_id(NextMethod("["))
}

#' @importFrom pillar pillar_shaft new_pillar_shaft_simple
#' @export

pillar_shaft.rhub_column_id <- function(x, ...) {
  cx <- shorten_rhub_id(x)
  new_pillar_shaft_simple(cx, ...)
}

#' @importFrom pillar type_sum
#' @export

type_sum.rhub_column_id <- function(x) {
  "rhub::id"
}
