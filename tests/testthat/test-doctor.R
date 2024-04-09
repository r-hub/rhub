test_that("rhub_doctor", {
  withr::local_options(cli.ansi = FALSE)

  # we do this here, so the web server process starts witg the same
  # working directory as the tests
  http$url()

  # we need to do this because we are wrapping text and also using
  # `transform` in `export_snapshot()`.
  withr::local_options(cli.width = Inf)

  # Do everything in a temporary package
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dir.create(tmp)
  file.copy(test_path("fixtures/pkg"), tmp, recursive = TRUE)
  pkg <- file.path(tmp, "pkg")
  withr::local_dir(pkg)

  # must be a git repo as well
  dir.create(".git")

  withr::local_envvar(RHUB_WORKFLOW_URL = http$url("/rhub.yaml"))
  expect_snapshot({
    rhub_setup()
  }, transform = redact_abs_path)

  mockery::stub(rhub_doctor, "doctor_find_gh_url", "https://github.com/r-lib/ps")
  mockery::stub(rhub_doctor, "doctor_find_pat", "secret")
  mockery::stub(rhub_doctor, "doctor_async_gql", list())
  mockery::stub(rhub_doctor, "doctor_async_rest", list())
  mockery::stub(rhub_doctor, "doctor_check_github", NULL)
  mockery::stub(rhub_doctor, "doctor_check_pat_scopes", NULL)
  mockery::stub(rhub_doctor, "doctor_check_workflow", NULL)

  expect_snapshot({
    rhub_doctor()
  }, transform = redact_abs_path)
})

test_that("doctor_find_gh_url", {
  # error
  mockery::stub(
    doctor_find_gh_url,
    "gert::git_info",
    list(remote = NA_character_)
  )
  expect_snapshot(error = TRUE, {
    doctor_find_gh_url(".")
  })

  # success
  mockery::stub(
    doctor_find_gh_url,
    "gert::git_info",
    list(remote = "origin")
  )
  mockery::stub(
    doctor_find_gh_url,
    "gert::git_remote_info",
    list(url = "https://github.com/blahblah")
  )
  expect_equal(doctor_find_gh_url("."), "https://github.com/blahblah")
})

test_that("doctor_find_pat", {
  withr::local_options(cli.ansi = FALSE)

  # no git
  mockery::stub(
    doctor_find_pat,
    "gitcreds::gitcreds_get",
    function(...) {
      throw(pkg_error("oops", .class = "gitcreds_nogit_error"))
    }
  )
  expect_snapshot(error = TRUE, {
    doctor_find_pat("https://github.com")
  })

  # no credentials
  mockery::stub(
    doctor_find_pat,
    "gitcreds::gitcreds_get",
    function(...) {
      throw(pkg_error("oops", .class = "gitcreds_no_credentials"))
    }
  )
  expect_snapshot(error = TRUE, {
    doctor_find_pat("https://github.com")
  })

  # other error
  mockery::stub(
    doctor_find_pat,
    "gitcreds::gitcreds_get",
    function(...) {
      throw(pkg_error("oops"))
    }
  )
  expect_snapshot(error = TRUE, {
    doctor_find_pat("https://github.com")
  })

  # ok
  mockery::stub(
    doctor_find_pat,
    "gitcreds::gitcreds_get",
    list(password = "secret")
  )
  expect_snapshot({
    doctor_find_pat("https://github.com")
  })
})

test_that("doctor_check_github", {
  withr::local_options(cli.ansi = FALSE)

  # ok
  resp <- list(headers = list("x-ratelimit-limit" = 10000))
  expect_snapshot({
    doctor_check_github("https://github.com/r-lib/ps", resp)
  })

  # not ok
  resp2 <- list(headers = list())
  expect_snapshot(error = TRUE, {
    doctor_check_github("https://github.com/r-lib/ps", resp2)
  })
})

test_that("doctor_check_pat_scopes", {
  withr::local_options(cli.ansi = FALSE)

  # no pat?
  resp <- list(headers = list())
  expect_snapshot(error = TRUE, {
    doctor_check_pat_scopes(resp)
  })

  # bad scopes
  resp2 <- list(headers = list("x-oauth-scopes" = "foo, bar"))
  expect_snapshot(error = TRUE, {
    doctor_check_pat_scopes(resp2)
  })

  # ok
  resp3 <- list(headers = list("x-oauth-scopes" = "foo, repo, bar"))
  expect_snapshot({
    doctor_check_pat_scopes(resp3)
  })
})

test_that("doctor_check_workflow", {
  withr::local_options(cli.ansi = FALSE)
  url <- "https://github.com/r-lib/ps"

  # no workflow
  expect_snapshot(error = TRUE, {
    doctor_check_workflow(url, list(), list())
  })

  # no workflow, forked
  expect_snapshot(error = TRUE, {
    doctor_check_workflow(url, list(is_forked = TRUE), list())
  })

  # not active
  expect_snapshot(error = TRUE, {
    doctor_check_workflow(
      url,
      list(workflow = "ok"),
      list(workflow = list(state = "bad"))
    )
  })

  # ok
  expect_snapshot({
    doctor_check_workflow(
      url,
      list(workflow = "ok"),
      list(workflow = list(state = "active"))
    )
  })
})

test_that("doctor_async_gql", {
  resp <- readRDS(test_path("fixtures/gh-response-doctor-gql.rds"))
  mockery::stub(
    doctor_async_gql,
    "async_gh_gql_get",
    function(...) async_constant(resp)
  )
  expect_snapshot({
    synchronise(doctor_async_gql("https://github.com/r-lib/ps", "secret"))
  })
})

test_that("doctor_async_rest", {
  resp <- readRDS(test_path("fixtures/gh-response-doctor-rest.rds"))
  mockery::stub(
    doctor_async_rest,
    "async_gh_rest_get",
    function(...) async_constant(resp)
  )
  expect_snapshot({
    synchronise(doctor_async_rest("https://github.com/r-lib/ps", "secret"))
  })
})