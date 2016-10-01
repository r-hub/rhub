
context("utils")

test_that("update", {
  expect_equal(
    update(list(a = 10, b = 20), list(a = 100, c = 5)),
    list(a = 100, b = 20, c = 5)
  )

  expect_equal(
    update(list(), list(a = 1, b = 2)),
    list(a = 1, b = 2)
  )

  expect_equal(
    update(list(a = 1, b = 2), list()),
    list(a = 1, b = 2)
  )
})

test_that("parse_email", {
  expect_equal(
    parse_email("first second <mail@foo.com>"),
    "mail@foo.com"
  )

  expect_equal(
    parse_email("the is no email here"),
    NA_character_
  )

  expect_equal(
    parse_email("<just-email@foo.com>"),
    "just-email@foo.com"
  )
})

test_that("get_maintainer_email", {
  pkg <- create_minimal_package()
  targz <- build_package(pkg, tempfile())

  expect_equal(get_maintainer_email(pkg), "first.second@foo.bar")
  expect_equal(get_maintainer_email(targz), "first.second@foo.bar")
})
