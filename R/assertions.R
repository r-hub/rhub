
#' @importFrom assertthat assert_that on_failure<-

is_pkg_dir <- function(path) {
  file.exists(path) &&
    file.info(path)$isdir &&
    file.exists(file.path(path, "DESCRIPTION"))
}

is_pkg_tarball <- function(path) {
  file.exists(path) &&
    grepl("\\.tar\\.gz", path)
}

is_pkg_dir_or_tarball <- function(path) {
  is_pkg_tarball(path) || is_pkg_dir(path)
}

on_failure(is_pkg_dir_or_tarball) <- function(call, env) {
  paste0(
    deparse(call$path),
    " is not an R package directory or source R package"
  )
}

is_string <- function(x) {
  !is.null(x) &&
    is.character(x) &&
    length(x) == 1 &&
    !is.na(x)
}

on_failure(is_string) <- function(call, env) {
  paste0(deparse(call$x), " is not a string")
}

is_string_or_null <- function(x) {
  is_string(x) || is.null(x)
}

on_failure(is_string_or_null) <- function(call, env) {
  paste0(deparse(call$x), " is not a string and not NULL")
}

is_email <- function(x) {
  assert_that(is_string(x))
  grepl(".@.", x)
}

on_failure(is_email) <- function(call, env) {
  paste0(deparse(call$x), " is not an email address")
}

is_flag <- function(x) {
  !is.null(x) &&
    is.logical(x) &&
    length(x) == 1 &&
    !is.na(x)
}

on_failure(is_flag) <- function(call, env) {
  paste0(deparse(call$x), " is not a flag (length one logical)")
}

is_named <- function(x) {
  length(names(x)) == length(x) &&
    all(names(x) != "")
}

on_failure(is_named) <- function(call, env) {
  paste0(deparse(call$x), " does not have names")
}

is_token <- function(x) {
  assert_that(is_string(x))
  grepl("[a-zA-Z0-9]{6}", x, perl = TRUE)
}

on_failure(is_token) <- function(call, env) {
  paste0(deparse(call$x), " does not look like an R-hub token")
}

is_check_ids <- function(x) {
  is.character(x) && length(x) >= 1 && all(x != "")
}

on_failure(is_check_ids) <- function(call, env) {
  paste0(deparse(call$x), " is not a vector of check ids")
}

is_count <- function(x) {
  is.numeric(x) && length(x) == 1 && as.integer(x) == x
}

on_failure(is_count) <- function(call, env) {
  paste0(deparse(call$x), " is not a count (length 1 integer)")
}

as_timeout <- function(x) {
  if (inherits(x, "difftime")) return(x)
  as.difftime(as.double(x), units = "secs")
}

is_timeout <- function(x) {
  inherits(x, "difftime")  && length(x) == 1  && !is.na(x)
}

on_failure(is_timeout) <- function(call, env) {
  paste0(deparse(call$x), " must be a timeout, a 'difftime' constant")
}
