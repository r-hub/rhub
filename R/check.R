
#' Check an R package on r-hub
#' @export
#' @importFrom desc desc_get_maintainer

check <- function(path = ".", platform = platforms()$name[1],
                  email = NULL, valgrind = FALSE,
                  show_status = interactive()) {

  ## Check that it is a package
  path <- normalizePath(path)
  assert_pkg_dir(path)
  assert_flag(valgrind)

  ## Make sure that maintainer email was validated
  if (is.null(email)) email <- parse_email(desc_get_maintainer(path))
  assert_validated_email(email)

  ## Build the tar.gz
  header_line("Building package")
  pkg_targz <- build_package(path, tmpdir <- tempfile())

  ## Create check_args
  check_args <- as.character(c(
    if (valgrind) "--use-valgrind"
  ))

  ## Submit to r-hub
  id <- submit_package(
    email,
    pkg_targz,
    platform = platform,
    check_args = check_args
  )

  ## Show the status
  check_status(id, interactive = show_status)
}
