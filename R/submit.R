
#' @importFrom rematch re_match
#' @importFrom base64enc base64encode

submit_package <- function(email, pkg_targz, platform) {

  assert_string(email)
  assert_string(platform)

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
      platform = unbox(platform),
      file = unbox(base64encode(pkg_targz))
    )
  )
}
