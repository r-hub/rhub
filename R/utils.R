
update <- function(original, new) {

  if (length(new)) {
    if (length(original)) assert_named(original)
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

#' @importFrom desc desc_get_maintainer
#' @importFrom utils untar

get_maintainer_email <- function(path) {
  path <- normalizePath(path)
  if (is_dir(path)) {
    parse_email(desc_get_maintainer(path))
  } else {
    dir.create(tmp <- tempfile())
    files <- untar(path, list = TRUE)
    desc <- grep("^[^/]+/DESCRIPTION$", files, value = TRUE)
    if (length(desc) < 1) stop("No 'DESCRIPTION' file in package")
    untar(path, desc, exdir = tmp)
    parse_email(desc_get_maintainer(file.path(tmp, desc)))
  }
}

`%:::%` <- function(p, f) {
  get(f, envir = asNamespace(p))
}

is_interactive <- function() {
  interactive()
}

is_dir <- function(x) {
  file.info(x)$isdir
}
