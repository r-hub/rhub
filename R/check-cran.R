
#' Check an R-package on R-hub, for a CRAN submission
#'
#' This function calls [check()] with arguments and platforms, that
#' are suggested for a CRAN submission.
#'
#' In particular, if `platforms` is `NULL` (the default), then
#' * It checks the package on Windows, and Linux.
#' * It checks the package on R-release and R-devel.
#' * It uses the `--as-cran` argument to `R CMD check`.
#' * It requires all dependencies, including suggested ones.
#' 
#' @details This function is wrapped by `devtools::check_rhub()` which you 
#' might find useful if you load `devtools` via your .Rprofile (see `usethis::use_devtools()`).
#'
#' @param check_args Arguments for `R CMD check`. By default `--as-cran`
#'   is used.
#' @param env_vars Character vecctor of environment variables to set on the builder. 
#'   By default `_R_CHECK_FORCE_SUGGESTS_="true"` is set, to require all packages used.
#'   `_R_CHECK_CRAN_INCOMING_USE_ASPELL_="true"` is also set, to use the
#'   spell checker.
#' @param ... Additional arguments are passed to [check()].
#' @inheritParams check
#' @return An [rhub_check] object.
#'
#' @export
#' @examples
#' \dontrun{
#' ch <- check_for_cran("package", show_status = FALSE)
#' ch$update()
#' ch$livelog(3)
#' }

check_for_cran <- function(
  path = ".", email = NULL, check_args = "--as-cran",
  env_vars = c("_R_CHECK_FORCE_SUGGESTS_" = "true",
    "_R_CHECK_CRAN_INCOMING_USE_ASPELL_" = "true"), platforms = NULL,
  ...) {

  path <- normalizePath(path)
  assert_that(is_pkg_dir_or_tarball(path))

  platforms <- platforms %||% default_cran_check_platforms(path)

  check(path = path, platforms = platforms, email = email,
        check_args = check_args, env_vars = env_vars, ...)
}

default_cran_check_platforms <- function(path) {
  c(
    "windows-x86_64-devel",
    "ubuntu-gcc-release",
    "fedora-clang-devel",
    if (needs_compilation(path)) "linux-x86_64-rocker-gcc-san"
  )
}
