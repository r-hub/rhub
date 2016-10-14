
#' List R-hub builds
#'
#' If both `email` and `package` are `NULL`, and the current directory
#' is an R package, then the checks of this package are queried, using
#' the maintainer's email address.
#'
#' If both `email` and `package` are `NULL`, and the current directory
#' is not an R package, then email address is guessed using
#' [whoami::email_address()] and all checks that belong to this address
#' are queried.
#'
#' If `package` is `NULL`, but `email` is not, then all builds of the
#' specified email address are queried.
#'
#' If `email` is `NULL`, but `package` is not, then `package` is
#' interpreted as a path, and the R package at that path is used, with
#' the maintainer's email address.
#'
#' If neither `email` nor `package` are `NULL`, then checks for the
#' specified email address and package are listed.
#'
#' If you want to use this function programatically, make sure you set
#' both `email` and `package`, to avoid context dependent behavior.
#'
#' @param email email address, or `NULL`, see details below.
#' @param package package name or `NULL`, see details below.
#'
#' @export
#' @importFrom desc desc_get

list_checks <- function(email = NULL, package = NULL) {
  in_pkg_dir <- is_pkg_dir(".")

  if (is.null(email) && is.null(package) && in_pkg_dir) {
    email <- get_maintainer_email(".")
    package <- unname(desc_get("Package", file = "."))

  } else if (is.null(email) && is.null(package)) {
    email <- email_address()

  } else if (is.null(package)) {
    ## Nothing to do

  } else if (is.null(email)) {
    email <- get_maintainer_email(package)
    package <- unname(desc_get("Package", file = package))
  }

  assert_validated_email_for_check(email)

  response <- query(
    "LIST BUILDS",
    data = drop_nulls(list(
      email = unbox(email),
      token = unbox(email_get_token(email)),
      package = package
    ))
  )

  class(response) <- "rhub_check_list"

  for (i in seq_along(response)) class(response[[i]]) <- "rhub_status"

  response
}

#' @export

print.rhub_check_list <- function(x, ...) {
  package <- vapply(x, "[[", "", "package")
  version <- vapply(x, "[[", "", "version")
  status <- vapply(x, "[[", "", "status")
  submitted <- vapply(x, "[[", "", "submitted")
  platform <- vapply(x, function(xx) xx$platform$name, "")

  tdiff <- Sys.time() - parse_iso_8601(submitted)
  units(tdiff) <- "secs"
  submitted <- paste(
    pretty_ms(as.numeric(tdiff) * 1000, compact = TRUE),
    "ago"
  )

  print(data_frame(
    package = package,
    version = version,
    status = status,
    submitted = submitted,
    platform = platform
  ))
}
