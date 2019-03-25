
handle_id <- function(x) {
  if (is.character(x)) {
    unname(sub("^.*/([^/]+)$", "\\1", x, perl = TRUE))
  } else if (inherits(x, "rhub_handle")) {
    unname(vapply(x, "[[", "", "id"))
  } else {
    stop("Invalid R-hub check id")
  }
}

#' @export

print.rhub_handle <- function(x, ...) {
  id <- handle_id(x)
  if (length(id) == 1) {
    cat("R-hub check: ", id, "\n", sep = "")

  } else {
    cat(
      "R-hub checks:\n",
      paste("  ", id, collapse = "\n")
    )
  }
  invisible(x)
}
