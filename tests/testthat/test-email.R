
context("email")

test_that("validate_email", {

  with_mock(
    `rhub::is_interactive` = function() FALSE,
    expect_error(validate_email("foo"))
  )

  args <- NULL
  with_mock(
    `rhub::email_add_token` = function(...) args <<- list(...),
    validate_email("email@domain", "token")
  )
  expect_equal(args, list("email@domain", "token"))

  args <- NULL
  with_mock(
    `base::readline` = function(...) "token",
    `rhub::is_interactive` = function() TRUE,
    `rhub::query` = function(...) NULL,
    `rhub::email_add_token` = function(...) args <<- list(...),
    validate_email("email@domain")
  )
  expect_equal(args, list("email@domain", "token"))
})

test_that("email_file", {
  ## Cannot easily test the value
  expect_silent(email_file())
})

test_that("email_get_token, email_add_token", {
  tmp <- tempfile()
  with_mock(
    `rhub::email_file` = function() tmp,
    {
      expect_null(email_get_token("bugs.bunny@acme.com"))
      email_add_token("bugs.bunny@acme.com", "token")
      expect_equal(email_get_token("bugs.bunny@acme.com"), "token")
      expect_null(email_get_token("duffy.duck@acme.com"))

      email_add_token("bugs.bunny@acme.com", "token2")
      expect_equal(email_get_token("bugs.bunny@acme.com"), "token2")
      expect_null(email_get_token("duffy.duck@acme.com"))
    }
  )

})
