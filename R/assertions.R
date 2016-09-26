
assert_pkg_dir <- function(path) {
  stopifnot(
    file.exists(path),
    file.info(path)$isdir,
    file.exists(file.path(path, "DESCRIPTION"))
  )
}

assert_string <- function(x) {
  stopifnot(
    is.character(x),
    length(x) == 1,
    !is.na(x)
  )
}

#' @importFrom whoami email_address

assert_validated_email <- function(email = email_address(),
                                   ask = interactive()) {
  assert_string(email)
  code <- email_get_token(email)
  if (is.na(code)) {
    if (ask) {
      message(sQuote(email), " is not validated, validating now.")
      validate_email(email)
    } else {
      stop(sQuote(email), " is not validated")
    }
  }
}

assert_flag <- function(x) {
  stopifnot(
    is.logical(x),
    length(x) == 1,
    !is.na(x)
  )
}
