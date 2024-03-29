
#' Check an R package on R-hub
#'
#' @param path Path to a directory containing an R package, or path to
#'   source R package tarball built with `R CMD build` or
#'   `devtools::build()`.
#' @param platforms A character vector of one or more platforms to build/check
#'   the package on. See [platforms()] for the available platforms. If this is
#'   \code{NULL}, and the R session is interactive, then a menu is shown. If it
#'   is \code{NULL}, and the session is not interactive, then the default R-hub
#'   platforms are used. A vector of platforms which saves time by building one
#'   R package tarball that is used for all the platforms specified.
#' @param email Email address to send notification to about the check.
#'   It must be a validated email address, see [validate_email()]. If
#'   `NULL`, then the email address of the maintainer is used, as defined
#'   in the `DESCRIPTION` file of the package.
#' @param valgrind Whether to run the check in valgrind. Only supported on
#'   Linux currently, and ignored on other platforms.
#' @param check_args Extra arguments for the `R CMD check` command.
#' @param env_vars Environment variables to set on the builder machine
#'   before the check. A named character vector.
#' @param show_status Whether to show the status of the build and check 
#' (live log) as it is happening.
#' @return An [rhub_check] object.
#'
#' @export
#' @examples
#' \dontrun{
#' check(".")
#' check("mypackage_1.0.0.tar.gz", platforms = "fedora-clang-devel")
#' }

check <- function(path = ".", platforms = NULL,
                  email = NULL, valgrind = FALSE, check_args = character(),
                  env_vars = character(), show_status = interactive()) {

  ## Check that it is a package
  path <- normalizePath(path)
  assert_that(is_pkg_dir_or_tarball(path))
  assert_that(is_flag(valgrind))
  assert_that(is_named(env_vars))
  assert_that(is.character(env_vars))

  ## Make sure that maintainer email was validated
  if (is.null(email)) email <- get_maintainer_email(path)
  if (is.na(email)) stop("Cannot get email address from package")
  assert_validated_email_for_check(email)

  platforms <- match_platform(platforms)

  ## Build the tar.gz, if needed
  if (file.info(path)$isdir) {
    header_line("Building package")
    pkg_targz <- build_package(path, tmpdir <- tempfile())
  } else {
    pkg_targz <- path
  }

  ## Add valgrind to check_args
  check_args <- c(
    check_args,
    if (valgrind) "--use-valgrind"
  )

  ## Submit to R-hub
  response <- submit_package(
    email,
    pkg_targz,
    platforms = platforms,
    check_args = check_args,
    env_vars = env_vars
  )

  ids <- vapply(response, "[[", "", "id")
  chk <- rhub_check$new(ids = ids)

  package_data$last_handle <- chk
  lapply(ids, cache_put, status = NULL)

  ## Show the live status, if requested
  if (show_status) chk$livelog()

  invisible(chk)
}

assert_validated_email_for_check <- function(email) {

  assert_that(is_email(email))
  code <- email_get_token(email)
  if (is.null(code)) {
    if (is_interactive()) {
      cat("\n")
      message(paste(collapse = "\n", strwrap(indent = 2, exdent = 2, paste0(
        sQuote(crayon::green(email)), " is not validated, or does not match ",
        "the package maintainer's email. To validate it now, please enter ",
        "the email address below. Note that R-hub will send a token to ",
        "this address. If the address does not belong to you, quit now by ",
        "pressing ", crayon::yellow("ENTER"), ". You can also specify a ",
        "different email by suppling email=."
      ))))
      cat("\n")
      email2 <- readline("  Email address: ")
      cat("\n")
      if (email2 == "") {
        stop("Aborting.", call. = FALSE)
      } else if (email != email2) {
        stop("Emails don't match, aborting", call. = FALSE)
      }
      validate_email(email)
    } else {
      stop(sQuote(email), " is not validated")
    }
  }
}
