
update <- function(original, new) {

  if (length(new)) {
    if (length(original)) assert_that(is_named(original))
    assert_that(is_named(new))
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
    if (!file.exists(file.path(path, "DESCRIPTION"))) {
      stop("No 'DESCRIPTION' file found")
    }
    parse_email(desc_get_maintainer(path))
  } else {
    dir.create(tmp <- tempfile())
    files <- untar(path, list = TRUE, tar = "internal")
    desc <- grep("^[^/]+/DESCRIPTION$", files, value = TRUE)
    if (length(desc) < 1) stop("No 'DESCRIPTION' file in package")
    untar(path, desc, exdir = tmp, tar = "internal")
    parse_email(desc_get_maintainer(file.path(tmp, desc)))
  }
}

needs_compilation <- function(path) {
  path <- normalizePath(path)
  if (is_dir(path)) {
    file.exists(file.path(path, "src"))
  } else {
    dir.create(tmp <- tempfile())
    files <- untar(path, list = TRUE, tar = "internal")
    any(grepl("^[^/]+/src/?$", files))
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

data_frame <- function(...) {
  data.frame(stringsAsFactors = FALSE, ...)
}

drop_nulls <- function(x) {
  x [ ! vapply(x, is.null, TRUE) ]
}
