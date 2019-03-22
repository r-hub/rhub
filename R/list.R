
#' List all checks for an email address
#'
#' @param email Email address. By default it is guessed with
#'   [whoami::email_address()]. The address must be validated, see
#'   [validate_email()].
#' @param package `NULL`, or a character scalar. Can be used to restrict
#'   the search for a single package.
#' @param howmany How many check groups (checks submitted simultaneously) to show. The current API limit is 20.
#' @return An [`rhub_check_list`] object.
#'
#' @export
#' @examples
#' \dontrun{
#' ch <- list_my_checks()
#' ch
#' ch$details()
#' }

list_my_checks <- function(email = email_address(), package = NULL,
                           howmany = 20) {

  assert_that(is_email(email))
  assert_that(is_string_or_null(package))
  assert_that(is_count(howmany))

  response <- if (is.null(package)) {
    query(
      "LIST BUILDS EMAIL",
      params = list(email = email, token = email_get_token(email)))
  } else {
    query(
      "LIST BUILDS PACKAGE",
      params = list(email = email, package = package,
                    token = email_get_token(email)))
  }

  if (length(response) > howmany) response <- response[seq_len(howmany)]

  make_check_list(response)
}


#' List checks of a package
#'
#' @param package Directory of an R package, or a package tarball.
#' @param email Email address that was used for the check(s).
#'   If `NULL`, then the maintainer address is used.
#' @param howmany How many checks to show. The current maximum of the API
#'   is 20.
#' @return An [`rhub_check_list`] object.
#'
#' @export
#' @importFrom desc desc_get
#' @examples
#' \dontrun{
#' ch <- list_my_checks()
#' ch
#' ch$details(1)
#' }

list_package_checks <- function(package = ".", email = NULL, howmany = 20) {

  assert_that(is_pkg_dir_or_tarball(package))
  if (is.null(email)) email  <- get_maintainer_email(package)
  assert_that(is_email(email))
  assert_that(is_count(howmany))

  package <- unname(desc_get("Package", file = package))

  response <- query(
    "LIST BUILDS PACKAGE",
    params = list(email = email, package = package,
                  token = email_get_token(email))
  )

  if (length(response) > howmany) response <- response[seq_len(howmany)]

  make_check_list(response)
}

make_check_list <- function(response) {
  data <- unlist(response, recursive = FALSE)

  df <- tibble::tibble(
    package = map_chr(data, "[[", "package"),
    version = map_chr(data, "[[", "version"),
    status = map_chr(data, "[[", "status"),
    group = map_chr(data, "[[", "group"),
    id = map_chr(data, "[[", "id"),
    result = map(data, function(x) x$result),
    email = map_chr(data, "[[", "email"),
    submitted = map_chr(data, "[[", "submitted"),
    started = map_chr(data, function(x) x$started %||% NA_character_),
    build_time = map_int(data, function(x) {
      suppressWarnings(as.integer(x$build_time)) %||% NA_integer_
    }),
    platform_name = map_chr(data, function(x) x$platform$name),
    platform = map(data, "[[", "platform"),
    builder = map_chr(data, function(x) x$builder_machine %||% NA_character_)
  )

  df
}
