
context("email")

test_that("validate_email", {

  with_mock(
    `rhub::is_interactive` = function() FALSE,
    expect_error(validate_email("foo"))
  )
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
      email_add_token("bugs.bunny@acme.com", "tokenxxx")
      expect_equal(email_get_token("bugs.bunny@acme.com"), "tokenxxx")
      expect_null(email_get_token("duffy.duck@acme.com"))

      email_add_token("bugs.bunny@acme.com", "token2")
      expect_equal(email_get_token("bugs.bunny@acme.com"), "token2")
      expect_null(email_get_token("duffy.duck@acme.com"))
    }
  )

})

test_that("validate_email assertions", {

  ## not an email address
  expect_error(validate_email("foo", "bar"))

  ## not a valid token
  expect_error(validate_email("foo@dom", ""))
  expect_error(validate_email("foo@dom", "bar"))
})
