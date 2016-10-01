
#' @importFrom withr with_dir
#' @importFrom callr rcmd_safe

build_package <- function(path, tmpdir) {

  path <- normalizePath(path)

  dir.create(tmpdir)
  file.copy(path, tmpdir, recursive = TRUE)

  ## If not a tar.gz, build it. Otherwise just leave it as it is.
  if (file.info(path)$isdir) {
    build_status <- with_dir(
      tmpdir,
      rcmd_safe("build", basename(path))
    )
    unlink(file.path(tmpdir, basename(path)), recursive = TRUE)
    report_system_error("Build failed", build_status)
  }

  file.path(
    tmpdir,
    list.files(tmpdir, pattern = "\\.tar\\.gz$")
  )
}
