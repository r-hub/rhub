test_that("rc_new_token", {
  # need interactive if missing arg
  withr::local_options(rlib_interactive = FALSE)
  expect_snapshot(error = TRUE, {
    rc_new_token()
    rc_new_token(email = "user@example.com")
    rc_new_token(token = "secret")
  })

  # calls interactive function in interactive mode
  withr::local_options(rlib_interactive = TRUE)
  called <- FALSE
  mockery::stub(
    rc_new_token,
    "rc_new_token_interactive",
    function(...) called <<- TRUE
  )
  rc_new_token()
  expect_true(called)

  # otherwise adds the supplied token
  args <- NULL
  mockery::stub(rc_new_token, "email_add_token", function(email, token) {
    args <<- list(email, token)
  })
  mockery::stub(rc_new_token, "email_file", "<email-file>")
  expect_snapshot({
    rc_new_token("user@example.com", "secret")
  })
  expect_equal(args, list("user@example.com", "secret"))
})

test_that("rc_list_local_tokens", {
  mockery::stub(rc_list_local_tokens, "list_validated_emails2", 113)
  expect_equal(rc_list_local_tokens(), 113)
})

test_that("rc_list_repos", {
  mockery::stub(
    rc_list_repos,
    "get_auth_header",
    c("Authorization" = "Bearer secret")
  )
  resp <- readRDS(test_path("fixtures/rc-response-list-repos.rds"))
  mockery::stub(rc_list_repos, "query", resp)
  expect_snapshot({
    rc_list_repos(email = "csardi.gabor@gmail.com")
  })
})

test_that("rc_submit", {
  # confirmation is non-interactive
  withr::local_options(rlib_interactive = FALSE)
  expect_snapshot(error = TRUE, {
    rc_submit()
  })

  mockery::stub(rc_submit, "select_platforms", c("linux", "clang18"))
  mockery::stub(rc_submit, "random_id", "kleptomaniac-harlequinbug")
  resp <- readRDS(test_path("fixtures/rc-response-submit.rds"))
  mockery::stub(rc_submit, "query", resp)
  mockery::stub(rc_submit, "get_auth_header", c("Authorization" = "Bearer token"))
  pkg <- test_path("fixtures/pkg_0.0.0.9000.tar.gz")

  expect_snapshot({
    rc_submit(pkg, confirmation = TRUE)
    (rc_submit(pkg, confirmation = TRUE))
  })

  # confirmation, abort
  withr::local_options(rlib_interactive = TRUE)
  ans <- "no"
  mockery::stub(rc_submit, "readline", function(prompt) {
    cat(prompt)
    cat(ans)
    ans
  })
  expect_snapshot(error = TRUE, {
    (rc_submit(pkg))
  })

  # confirmation, yes
  ans <- "yes"
  expect_snapshot({
    (rc_submit(pkg))
  })

  # bad package file
  pkg <- test_path("fixtures/bad.tar.gz")
  expect_snapshot(error = TRUE, {
    rc_submit(pkg)
  })

  # need to build if directory
  withr::local_options(useFancyQuotes = FALSE)
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dir.create(tmp)
  file.copy(test_path("fixtures/pkg"), tmp, recursive = TRUE)
  pkg <- file.path(tmp, "pkg")
  expect_snapshot({
    (rc_submit(pkg))
  }, transform = function(x) {
    x <- sub(
      "checking for file .*[.][.][.]",
      "checking for file '<path> ...",
      x
    )
    x <- gsub("\x91", "'", x, fixed = TRUE, useBytes = TRUE)
    x <- gsub("\x92", "'", x, fixed = TRUE, useBytes = TRUE)
    x <- gsub("\xe2\x80\x98", "'", x, fixed = TRUE, useBytes = TRUE)
    x <- gsub("\xe2\x80\x99", "'", x, fixed = TRUE, useBytes = TRUE)
    x
  })

  # error response
  mockery::stub(rc_submit, "query", list())
  pkg <- test_path("fixtures/pkg_0.0.0.9000.tar.gz")
  expect_snapshot(error = TRUE, {
    (rc_submit(pkg))
  })
})

# == Internals ============================================================

test_that("guess_email", {
  # maint
  mockery::stub(guess_email, "get_maintainer_email", "user@example.com")
  expect_snapshot({
    guess_email()
    guess_email(message = FALSE)
  })

  mockery::stub(
    guess_email,
    "get_maintainer_email",
    function(...) stop("no")
  )
  mockery::stub(guess_email, "email_address", "another@example.com")
  expect_snapshot(guess_email())
  expect_snapshot(guess_email(message = FALSE))
})

test_that("get_auth_header", {
  valid <- read_token_file(test_path("fixtures/validated_emails.csv"))
  mockery::stub(get_auth_header, "list_validated_emails2", valid)

  # ok
  expect_snapshot({
    get_auth_header("csardi.gabor@gmail.com")
  })

  # not ok
  expect_snapshot(error = TRUE, {
    get_auth_header("user@example.com")
  })
})

test_that("get_email_to_validate", {
  valid <- read_token_file(test_path("fixtures/validated_emails.csv"))
  mockery::stub(get_email_to_validate, "list_validated_emails2", valid)
  mockery::stub(get_email_to_validate, "email_address", "user@example.com")
  mockery::stub(get_email_to_validate, "get_maintainer_email", "maint@example.com")
  mockery::stub(get_email_to_validate, "menu", function(choices, title) {
    cat(title, sep = "\n\n")
    cat(paste0(seq_along(choices), ": ", choices), sep = "\n")
    cat("\nSelection: ")
    cat(ans)
    ans
  })

  ans <- 0
  expect_snapshot(error = TRUE, {
    get_email_to_validate(".")
  })

  ans <- 5L
  expect_snapshot({
    get_email_to_validate(".")
  })

  mockery::stub(get_email_to_validate, "readline", function(prompt) {
    cat(prompt)
    ea <- "custom@example.com"
    cat(ea)
    ea
  })
  ans <- 7L
  expect_snapshot({
    get_email_to_validate(".")
  })

  # single validated address
  mockery::stub(get_email_to_validate, "list_validated_emails2", valid[FALSE, ])
  mockery::stub(get_email_to_validate, "email_address", function() stop("no"))
  mockery::stub(get_email_to_validate, "get_maintainer_email", function() stop("no"))
  expect_snapshot({
    get_email_to_validate(".")
  })
})

test_that("list_validated_emails2", {
  ef <- test_path("fixtures/validated_emails.csv")
  mockery::stub(list_validated_emails2, "email_file", ef)
  expect_snapshot({
    list_validated_emails2(message = FALSE)
    list_validated_emails2(message = TRUE)
  })

  ef <- tempfile()
  mockery::stub(list_validated_emails2, "email_file", ef)
  withr::local_options(rlib_interactive = TRUE)
  expect_snapshot({
    list_validated_emails2(message = FALSE)
    list_validated_emails2(message = TRUE)
  })
})

test_that("email_file", {
  mockery::stub(email_file, "user_data_dir", "/config")
  expect_snapshot(email_file())
})

test_that("rc_new_token_interactive", {
  mockery::stub(rc_new_token_interactive, "query", "done")
  mockery::stub(rc_new_token_interactive, "readline", "token")
  mockery::stub(
    rc_new_token_interactive,
    "rc_new_token",
    function(email, token) list(email, token)
  )
  expect_snapshot({
    rc_new_token_interactive(email = "maint@example.com")
  })

  mockery::stub(rc_new_token_interactive, "get_email_to_validate", "user@example.com")
  expect_snapshot({
    rc_new_token_interactive()
  })
})

test_that("email_add_token", {
  tmp <- tempfile()
  ef <- file.path(tmp, "emails.csv")
  on.exit(unlink(tmp), add = TRUE)

  # file does not exist yet
  mockery::stub(email_add_token, "email_file", ef)
  email_add_token("newemail@example.com", "new-token")
  expect_snapshot(read_token_file(ef))

  # file exists already, append
  email_add_token("newemail2@example.com", "new-token2")
  expect_snapshot(read_token_file(ef))

  # file exists already, replace
  email_add_token("newemail@example.com", "new-new-token")
  expect_snapshot(read_token_file(ef))
})
