
#' @importFrom rematch re_match
#' @importFrom base64enc base64encode

submit_package <- function(email, pkg_targz) {

  m <- re_match(
    pattern = "^(?<package>.+)_(?<version>.+)\\.tar\\.gz",
    basename(pkg_targz)
  )

  query(
    "SUBMIT PACKAGE",
    list(
      email = unbox(email),
      token = unbox(email_get_token(email)),
      package = unbox(m[, "package"]),
      version = unbox(m[, "version"]),
      file = unbox(base64encode(pkg_targz))
    )
  )
}
