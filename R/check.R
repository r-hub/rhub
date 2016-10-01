
#' Check an R package on r-hub
#'
#' @param path Path to a directory containing an R package, or path to
#'   source R package tarball built with `R CMD check`.
#' @param platform Platform to build/check the package on. See
#'   [platforms()] for the available platforms.
#' @param email Email address to send notification to about the build.
#'   It must be a validated email address, see [validate_email()]. If
#'   `NULL`, then the email address of the maintainer is used, as defined
#'   in the `DESCRIPTION` file of the package.
#' @param valgrind Whether to run the check in valgrind. Only supported on
#'   Linux currently, and ignored on other platforms.
#' @param check_args Extra arguments for the `R CMD check` command.
#' @param show_status Whether to show the status of the build as it is
#'   happening.
#' @return Return the response from r-hub, invisibly. It contains the
#'   URL of the build's status page on r-hub.
#'
#' @export
#' @examples
#' \dontrun{
#' check(".")
#' check("mypackage_1.0.0.tar.gz", platform = "fedora-clang-devel")
#' }

check <- function(path = ".", platform = platforms()$name[1],
                  email = NULL, valgrind = FALSE, check_args = character(),
                  show_status = interactive()) {

  ## Check that it is a package
  path <- normalizePath(path)
  assert_pkg_dir_or_tarball(path)
  assert_flag(valgrind)

  ## Make sure that maintainer email was validated
  if (is.null(email)) email <- get_maintainer_email(path)
  if (is.na(email)) stop("Cannot get email address from package")
  assert_validated_email(email)

  ## Build the tar.gz, if needed
  if (file.info(path)$isdir) {
    if (show_status) header_line("Building package")
    pkg_targz <- build_package(path, tmpdir <- tempfile())
  } else {
    pkg_targz <- path
  }

  ## Add valgrind to check_args
  check_args <- c(
    check_args,
    if (valgrind) "--use-valgrind"
  )

  ## Submit to r-hub
  id <- submit_package(
    email,
    pkg_targz,
    platform = platform,
    check_args = check_args,
    show_status = show_status
  )

  ## Show the status
  check_status(id, interactive = show_status)
}
