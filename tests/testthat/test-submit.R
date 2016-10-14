
context("submit")

test_that("submit_package", {
  pkg <- create_minimal_package()
  pkg_targz <- build_package(pkg, tempfile())

  args <- NULL
  with_mock(
    `rhub::query` = function(...) args <<- list(...),
    `rhub::email_get_token` = function(...) "token",
    submit_package("e@d", pkg_targz, "platform", c("arg1", "arg2"),
                   c("env" = "var"), FALSE)
  )

  expect_identical(args[[1]], "SUBMIT PACKAGE")

  expect_identical(
    names(args[[2]]),
    c("email", "token", "package", "version", "platform", "env",
      "check_args", "file")
  )

  expect_identical(args[[2]]$email, jsonlite::unbox("e@d"))
  expect_identical(args[[2]]$token, jsonlite::unbox("token"))
  expect_identical(args[[2]]$package, jsonlite::unbox(basename(pkg)))
  expect_identical(args[[2]]$version, jsonlite::unbox("1.0.0"))
  expect_identical(args[[2]]$platform, jsonlite::unbox("platform"))
  expect_identical(args[[2]]$check_args, jsonlite::unbox("arg1 arg2"))

  ## Must be a base64 string
  expect_match(
    gsub("\\s+", "", args[[2]]$file),
    "^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?$",
    perl = TRUE
  )

  ## If we want output, then there is output
  expect_output(
    with_mock(
      `rhub::query` = function(...) args <<- list("status-url" = "url"),
      `rhub::email_get_token` = function(...) "token",
      submit_package("e@d", pkg_targz, "p", c("a1", "a"), show_status = TRUE)
    ),
    "Preparing build"
  )

})
