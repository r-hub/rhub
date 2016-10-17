
make_status_list <- function(response) {
  class(response) <- "rhub_check_list"
  for (i in seq_along(response)) class(response[[i]]) <- "rhub_status"
  response
}

#' List all checks for an email address
#'
#' @param email Email address. By default it is guessed with
#'   [whoami::email_address()]. The address must be validated, see
#'   [validate_email()].
#' @param package `NULL`, or a character scalar. Can be used to restrict
#'   the search for a single package.
#' @return List of check status objects.
#'
#' @export

list_my_checks <- function(email = email_address(), package = NULL) {

  assert_that(is_email(email))
  assert_that(is_string_or_null(package))

  response <- query(
    "LIST BUILDS",
    data = drop_nulls(list(
      email = unbox(email),
      token = unbox(email_get_token(email)),
      package = package
    ))
  )

  make_status_list(response)
}


#' List checks of a package
#'
#' @param package Directory of an R package, or a package tarball.
#' @param email Email address that was used for the check(s).
#'   If `NULL`, then the maintainer address is used.
#' @return List of check status objects.
#'
#' @export
#' @importFrom desc desc_get

list_package_checks <- function(package = ".", email = NULL) {

  assert_that(is_pkg_dir_or_tarball(package))
  if (is.null(email)) email  <- get_maintainer_email(package)
  assert_that(is_email(email))

  package <- unname(desc_get("Package", file = package))

  response <- query(
    "LIST BUILDS",
    data = drop_nulls(list(
      email = unbox(email),
      token = unbox(email_get_token(email)),
      package = package
    ))
  )

  make_status_list(response)
}

#' @export

print.rhub_check_list <- function(x, ...) {
  package <- vapply(x, "[[", "", "package")
  version <- vapply(x, "[[", "", "version")
  status <- vapply(x, "[[", "", "status")
  submitted <- vapply(x, "[[", "", "submitted")
  platform <- vapply(x, function(xx) xx$platform$name, "")

  submitted <- if (length(package) == 0) {
    character()
  } else {
    tdiff <- Sys.time() - parse_iso_8601(submitted)
    units(tdiff) <- "secs"
    paste(
      pretty_ms(as.numeric(tdiff) * 1000, compact = TRUE),
      "ago"
    )
  }

  print(data_frame(
    package = package,
    version = version,
    status = status,
    submitted = submitted,
    platform = platform
  ))
}
