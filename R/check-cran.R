
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
#' @param env_vars Environment variables to set on the builder. By default
#'   `_R_CHECK_FORCE_SUGGESTS_=true` is set, to require all packages used.
#'   `_R_CHECK_CRAN_INCOMING_USE_ASPELL_=true` is also set, to use the
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
  env_vars = c(
    "_R_CHECK_CRAN_INCOMING_" = "true",
    "_R_CHECK_VC_DIRS_" = "true",
    "_R_CHECK_NO_RECOMMENDED_" = "true",
    "_R_CHECK_REPLACING_IMPORTS_" = "true",
    "_R_CHECK_S3_METHODS_NOT_REGISTERED_" = "true",
    "_R_CHECK_R_DEPENDS_" = "warn",
    "_R_CLASS_MATRIX_ARRARY_" = "true",
    "_R_CHECK_FORCE_SUGGESTS_" = "true",
    "_R_CHECK_RD_LINE_WIDTHS_" = "true",
    "_R_CHECK_EXECUTABLES_EXCLUSIONS_" = "false",
    "_R_CHECK_DOC_SIZES2_" = "true",
    "_R_CHECK_TIMINGS_" = "10",
    "_R_CHECK_INSTALL_DEPENDS_" = "true",
    "_R_CHECK_SUGGESTS_ONLY_" = "true",
    "_R_CHECK_DEPENDS_ONLY_DATA_" = "true",
    "_R_CHECK_CRAN_INCOMING_ " = "true",
    "_R_CHECK_CRAN_INCOMING_REMOTE_" = "true",
    "_R_CHECK_CODE_ASSIGN_TO_GLOBALENV_" = "true",
    "_R_CHECK_CODE_ATTACH_" = "true",
    "_R_CHECK_CODE_DATA_INTO_GLOBALENV_" = "true",
    "_R_CHECK_DOT_FIRSTLIB_" = "true",
    "_R_CHECK_DEPRECATED_DEFUNCT_" = "true",
    "_R_CHECK_SCREEN_DEVICE_" = "stop",
    "_R_CHECK_WINDOWS_DEVICE_" = "stop",
    "_R_CHECK_TOPLEVEL_FILES_" = "true",
    "_R_CHECK_LIMIT_CORES_" = "true",
    "_R_CHECK_CODE_USAGE_VIA_NAMESPACES_" = "true",
    "_R_CHECK_OVERWRITE_REGISTERED_S3_METHODS_" = "true",
    "_R_CHECK_NATIVE_ROUTINE_REGISTRATION_" = "true",
    "_R_CHECK_NO_STOP_ON_TEST_ERROR_" = "true",
    "_R_CHECK_PRAGMAS_" = "true",
    "_R_CHECK_COMPILATION_FLAGS_" = "true",
    "_R_CHECK_SERIALIZATION_" = "true",
    "_R_CHECK_R_ON_PATH_" = "true",
    "_R_CHECK_PACKAGES_USED_IN_TESTS_USE_SUBDIRS_" = "true",
    "_R_CHECK_SHLIB_OPENMP_FLAGS_" = "true",
    "_R_CHECK_CONNECTIONS_LEFT_OPEN_" = "true",
    "_R_CHECK_FUTURE_FILE_TIMESTAMPS_" = "true",
    "_R_CHECK_LENGTH_1_CONDITION_" = "package:_R_CHECK_PACKAGE_NAME_,[abort,]verbose",
    "_R_CHECK_LENGTH_1_LOGIC2_" = "package:_R_CHECK_PACKAGE_NAME_,[abort,]verbose",
    "_R_CHECK_SYSTEM_CLOCK_" = "false",
    "_R_CHECK_AUTOCONF_" = "true",
    "_R_CHECK_DATALIST_" = "true",
    "_R_CHECK_THINGS_IN_CHECK_DIR_" = "true",
    "_R_CHECK_THINGS_IN_TEMP_DIR_" = "true",
    "_R_CHECK_BASHISMS_" = "true",
    "_R_CHECK_ORPHANED_" = "true",
    "_R_CHECK_EXCESSIVE_IMPORTS_" = "20",
    "_R_CHECK_BOGUS_RETURN_" = "true",
    "_R_CHECK_CRAN_INCOMING_USE_ASPELL_" = "true",
    "_R_CHECK_CRAN_INCOMING_CHECK_FILE_URIS_" = "true"
    ), platforms = NULL,
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
