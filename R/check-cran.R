
#' Check an R-package on R-hub, for a CRAN submission
#'
#' This function calls [check()] with arguments and platforms, that
#' are suggested for a CRAN submission. In particular:
#' * It checks the package on Windows, and Linux.
#' * It checks the package on R-oldrel, R-release and R-devel.
#' * It uses the `--as-cran` argument to `R CMD check`.
#' * It requires all dependencies, including suggested ones.
#'
#' @param check_args Arguments for `R CMD check`. By default `--as-cran`
#'   is used.
#' @param env_vars Environment variables to set on the builder. By default
#'   `_R_CHECK_FORCE_SUGGESTS_=true` is set, to require all packages used.
#' @inheritParams check
#' @return Return the response from r-hub, invisibly. It contains the
#'   URL of the build's status page on r-hub.
#'
#' @export

check_for_cran <- function(
  path = ".", email = NULL, check_args = "--as-cran",
  env_vars = c("_R_CHECK_FORCE_SUGGESTS_" = "true"), ...) {

  path <- normalizePath(path)
  assert_that(is_pkg_dir_or_tarball(path))

  platforms <- c(
    "windows-x86_64-oldrel",
    "ubuntu-gcc-release",
    "fedora-clang-devel",
    if (needs_compilation(path)) "linux-x86_64-rocker-gcc-san"
  )

  check(path = path, platform = platforms, email = email,
        check_args = check_args, env_vars = env_vars, show_status = FALSE,
        ...)
}
