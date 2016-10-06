
#' @importFrom rematch re_match
#' @importFrom jsonlite base64_enc
#' @importFrom crayon blue

submit_package <- function(email, pkg_targz, platform, check_args,
                           show_status) {

  assert_that(is_email(email))
  assert_that(is_string_or_null(platform))

  m <- re_match(
    pattern = "^(?<package>.+)_(?<version>.+)\\.tar\\.gz",
    basename(pkg_targz)
  )

  if (show_status) header_line("Uploading package")
  buf <- readBin(pkg_targz, raw(), file.info(pkg_targz)$size)
  response <- query(
    "SUBMIT PACKAGE",
    list(
      email = unbox(email),
      token = unbox(email_get_token(email)),
      package = unbox(unname(m[, "package"])),
      version = unbox(unname(m[, "version"])),
      platform = unbox(platform),
      check_args = unbox(paste(check_args, collapse = " ")),
      file = unbox(base64_enc(buf))
    )
  )

  if (show_status) {
    header_line(paste0(
      "Preparing build, see status at\n   ",
      blue(response$`status-url`)
    ))
  }

  response
}
