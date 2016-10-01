
context("assertions")

test_that("is_pkg_dir", {
  tmppkg <- tempfile()
  dir.create(tmppkg)

  expect_false(is_pkg_dir(tmppkg))
  expect_error(assert_pkg_dir_or_tarball(tmppkg))

  cat("dummy\n", file = file.path(tmppkg, "DESCRIPTION"))
  expect_true(is_pkg_dir(tmppkg))
  expect_silent(assert_pkg_dir_or_tarball(tmppkg))
})

test_that("is_pkg_tarball", {
  tmp <- tempfile(fileext="dummy")
  cat("dummy\n", file = tmp)
  expect_false(is_pkg_tarball(tmp))
  expect_error(assert_pkg_dir_or_tarball(tmppkg))

  tmppkg <- tempfile(fileext = ".tar.gz")
  cat("dummy\n", file = tmppkg)
  expect_true(is_pkg_tarball(tmppkg))
  expect_silent(assert_pkg_dir_or_tarball(tmppkg))
})

test_that("assert_string", {
  pos <- list(
    "foo",
    ""
  )
  for (p in pos) expect_silent(assert_string(p))

  neg <- list(
    character(),
    1:10,
    c("foo", "bar"),
    NULL
  )
  for (n in neg) expect_error(assert_string(n))
})

test_that("assert_validated_email", {

  with_mock(
    `rhub::email_get_token` = function(x) "your-token",
    expect_silent(assert_validated_email("foobar"))
  )

  expect_error(assert_validated_email(
    basename(tempfile()),
    ask = FALSE
  ))

  with_mock(
    `rhub::validate_email` = function(x) "foobar",
    expect_equal(
      assert_validated_email(basename(tempfile()), ask = TRUE),
      "foobar"
    )
  )
})

test_that("assert_flag", {
  pos <- list(
    TRUE,
    FALSE
  )
  for (p in pos) expect_silent(assert_flag(p))

  neg <- list(
    logical(),
    c(TRUE, TRUE),
    "TRUE",
    NULL
  )
  for (n in neg) expect_error(assert_flag(n))
})

test_that("assert_named", {

  pos <- list(
    c(a = 1, b = 2, c = 3),
    structure(double(), names = character()),
    list(foo = 1),
    structure(list(), names = character())
  )
  for (p in pos) expect_silent(assert_named(p))

  neg <- list(
    1:5,
    c(a = 1, 2, c = 3),
    list(1, 2, 3),
    list(a = 1, 2, c = 3)
  )
  for (n in neg) expect_error(assert_named(n))
})
