
#' @importFrom rematch re_match
#' @importFrom jsonlite base64_enc
#' @importFrom crayon blue

submit_package <- function(email, pkg_targz, platform, check_args,
                           env_vars) {

  assert_that(is_email(email))
  assert_that(
    is.character(platform),
    length(platform) >= 1
  )

  m <- re_match(
    pattern = "^(?<package>.+)_(?<version>.+)\\.tar\\.gz",
    basename(pkg_targz)
  )

  header_line("Uploading package")
  buf <- readBin(pkg_targz, raw(), file.info(pkg_targz)$size)
  response <- query(
    "SUBMIT PACKAGE",
    data = list(
      email = unbox(email),
      token = unbox(email_get_token(email)),
      package = unbox(unname(m[, "package"])),
      version = unbox(unname(m[, "version"])),
      platform = platform,
      env = as.list(env_vars),
      check_args = unbox(paste(check_args, collapse = " ")),
      file = unbox(base64_enc(buf))
    )
  )

  header_line(paste0(
    "Preparing build, see status at\n",
    blue(paste(
      "  ",
      vapply(response, "[[", "", "status-url"),
      collapse = "\n"
      ))
  ))

  response
}
