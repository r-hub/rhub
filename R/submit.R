
#' @importFrom rematch re_match
#' @importFrom jsonlite base64_enc
#' @importFrom crayon blue

submit_package <- function(email, pkg_targz, platform) {

  assert_string(email)
  assert_string(platform)

  m <- re_match(
    pattern = "^(?<package>.+)_(?<version>.+)\\.tar\\.gz",
    basename(pkg_targz)
  )

  header_line("Uploading package")
  buf <- readBin(pkg_targz, raw(), file.info(pkg_targz)$size)
  response <- query(
    "SUBMIT PACKAGE",
    list(
      email = unbox(email),
      token = unbox(email_get_token(email)),
      package = unbox(m[, "package"]),
      version = unbox(m[, "version"]),
      platform = unbox(platform),
      file = unbox(base64_enc(buf))
    )
  )
  header_line(paste0(
    "Preparing build, see status at\n   ",
    blue(response$`status-url`)
  ))

  response
}
