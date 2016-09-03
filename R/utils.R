
update <- function(original, new) {

  if (length(new)) {
    assert_named(original)
    assert_named(new)
    original[names(new)] <- new
  }

  original
}

#' @importFrom rematch re_match

parse_email <- function(x) {
  unname(
    re_match(pattern = "<(?<email>[^>]+)>", x)[, "email"]
  )
}

`%||%` <- function(l, r) if (is.null(l)) r else l
