
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

get_group <- function(l){
  if (! "group" %in% names(l)){
    ""
  } else {
    l[["group"]]
  }
}

cat0 <- function(..., sep = "") {
  cat(..., sep = sep)
}

map <- function(.x, .f, ...) {
  lapply(.x, .f, ...)
}

map_lgl <- function(.x, .f, ...) {
  vapply(.x, .f, logical(1), ...)
}

map_chr <- function(.x, .f, ...) {
  vapply(.x, .f, character(1), ...)
}

map_int <- function(.x, .f, ...) {
  vapply(.x, .f, integer(1), ...)
}

shorten_rhub_id <- function(x) {
  sx <- strsplit(x, "-", fixed = TRUE)
  substr(map_chr(sx, tail, 1), 1, 7)
}

## This is a workaround to handle NAs

my_pretty_dt <- function(x, compact = TRUE) {
  res <- rep("?", length(x))
  res[!is.na(x)] <- pretty_dt(x[!is.na(x)], compact = compact)
  res
}

problem_statuses <- function(){
  c("parseerror", "preperror", "aborted")
}
