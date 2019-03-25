
#' List all checks for an email address
#'
#' @param email Email address. By default it is guessed with
#'   [whoami::email_address()]. The address must be validated, see
#'   [validate_email()].
#' @param package `NULL`, or a character scalar. Can be used to restrict
#'   the search for a single package.
#' @param howmany How many check groups (checks submitted simultaneously)
#'   to show. The current API limit is 20.
#' @return A [tibble::tibble] with columns:
#'   * package Name of the package.
#'   * version Package version.
#'   * result: More detailed result of the check. Can be `NULL` for errors.
#'     This is a list column with members: `status`, `errors`, `warnings`,
#'     `notes`.
#'   * group: R-hub check group id.
#'   * id: `R-hub check id.
#'   * platform_name: Name of the check platform.
#'   * build_time: Build time, a [difftime] object.
#'   * submitted: Time of submission.
#'   * started: Time of the check start.
#'   * platform: Detailed platform data, a list column.
#'   * builder: Name of the builder machine.
#'   * status Status of the check. Possible values:
#'     - `created`: check job was created, but not running yet.
#'     - `in-progress`: check job is running.
#'     - `parseerror`: internal R-hub error parsing the check results.
#'     - `preperror`: check error, before the package check has started.
#'     - `aborted`: aborted by admin or user.
#'     - `error`: failed check. (Possibly warnings and notes as well.)
#'     - `warning`: `R CMD check` reported warnings. (Possibly notes as well.)
#'     - `note`: `R CMD check` reported notes.
#'     - `ok`: successful check.
#'   * email: Email address of maintainer / submitter.
#'
#' @export
#' @seealso list_package_checks
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
#' @inherit list_my_checks return
#'
#' @export
#' @importFrom desc desc_get
#' @examples
#' \dontrun{
#' ch <- list_package_checks()
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
    result = column_result(map(data, function(x) x$result)),
    group = column_group_id(map_chr(data, "[[", "group")),
    id = column_id(map_chr(data, "[[", "id")),
    platform_name = map_chr(data, function(x) x$platform$name),
    build_time = column_dt(map_int(data, function(x) {
      suppressWarnings(as.integer(x$build_time %||% NA_integer_))
    })),
    submitted = column_time(map_chr(data, "[[", "submitted")),
    started = column_time(map_chr(data, function(x) x$started %||% NA_character_)),
    platform = map(data, "[[", "platform"),
    builder = map_chr(data, function(x) x$builder_machine %||% NA_character_),
    status = column_status(map_chr(data, "[[", "status")),
    email = map_chr(data, "[[", "email")
  )

  cache_put_ids(df$id)
  cache_put_group_ids(df$group)

  df
}

column_time <- function(x) {
  res <- rep(as.POSIXct(NA_character_), length(x))
  res[! is.na(x)] <- parse_iso_8601(x[!is.na(x)])
  res
}
