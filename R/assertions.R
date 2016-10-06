
is_pkg_dir <- function(path) {
  file.exists(path) &&
    file.info(path)$isdir &&
    file.exists(file.path(path, "DESCRIPTION"))
}

is_pkg_tarball <- function(path) {
  file.exists(path) &&
    grepl("\\.tar\\.gz", path)
}

assert_pkg_dir_or_tarball <- function(path) {
  stopifnot(
    is_pkg_tarball(path) || is_pkg_dir(path)
  )
}

assert_string <- function(x) {
  stopifnot(!is.null(x))
  stopifnot(
    is.character(x),
    length(x) == 1,
    !is.na(x)
  )
}

assert_email <- function(x) {
  assert_string(x)
  stopifnot(grepl(".@.", x))
}

#' @importFrom whoami email_address

assert_validated_email <- function(email = email_address(),
                                   ask = interactive()) {
  assert_string(email)
  code <- email_get_token(email)
  if (is.null(code)) {
    if (ask) {
      message(sQuote(email), " is not validated, validating now.")
      validate_email(email)
    } else {
      stop(sQuote(email), " is not validated")
    }
  }
}

assert_flag <- function(x) {
  stopifnot(!is.null(x))
  stopifnot(
    is.logical(x),
    length(x) == 1,
    !is.na(x)
  )
}

assert_named <- function(x) {
  stopifnot(
    !is.null(names(x)),
    all(names(x) != "")
  )
}

assert_token <- function(x) {
  assert_string(x)
  stopifnot(grepl("[a-zA-Z0-9]{6}", x, perl = TRUE))
}
