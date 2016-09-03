
#' Check an R package on r-hub
#' @export
#' @importFrom desc desc_get_maintainer

check <- function(path = ".", platform = platforms()$name[1],
                  email = NULL, show_status = interactive()) {

  ## Check that it is a package
  path <- normalizePath(path)
  assert_pkg_dir(path)

  ## Make sure that maintainer email was validated
  if (is.null(email)) email <- parse_email(desc_get_maintainer(path))
  assert_validated_email(email)

  ## Build the tar.gz
  pkg_targz <- build_package(path, tmpdir <- tempfile())

  ## Submit to r-hub
  id <- submit_package(email, pkg_targz)

  ## Show the status
  check_status(id, interactive = show_status)
}
