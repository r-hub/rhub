test_that("check_rpkg_root", {
  expect_silent(check_rpkg_root("/foo/bar", "/foo/bar"))
  expect_snapshot(error = TRUE, {
    check_rpkg_root("/pkg/root", "/git/root")
  })
})

test_that("rhub_setup", {
  withr::local_options(cli.ansi = FALSE)

  # we do this here, so the web server process starts witg the same
  # working directory as the tests
  http$url()

  # we need to do this because we are wrapping text and also using
  # `transform` in `export_snapshot()`.
  withr::local_options(cli.width = Inf)

  # check this before changing wd
  wf_hash <- cli::hash_file_sha1(test_path("fixtures/rhub.yaml"))

  # Do everything in a temporary package
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dir.create(tmp)
  file.copy(test_path("fixtures/pkg"), tmp, recursive = TRUE)
  pkg <- file.path(tmp, "pkg")
  withr::local_dir(pkg)

  # must be a git repo as well
  dir.create(".git")

  # fails to download workflow file
  withr::local_envvar(RHUB_WORKFLOW_URL = http$url("/badbadbad"))
  expect_snapshot(error = TRUE, {
    rhub_setup()
  }, transform = function(x) redact_abs_path(redact_port(x)))

  # no workflow file, copy there
  withr::local_envvar(RHUB_WORKFLOW_URL = http$url("/rhub.yaml"))
  expect_snapshot({
    rhub_setup()
  }, transform = redact_abs_path)
  expect_equal(cli::hash_file_sha1(".github/workflows/rhub.yaml"), wf_hash)

  # workflow file is up to date
  expect_snapshot({
    rhub_setup()
  }, transform = redact_abs_path)
  expect_equal(cli::hash_file_sha1(".github/workflows/rhub.yaml"), wf_hash)

  # workflow file is outdated
  cat("This is a change", file = ".github/workflows/rhub.yaml")
  wf_upd_hash <- cli::hash_file_sha1(".github/workflows/rhub.yaml")
  expect_snapshot(error = TRUE, {
    rhub_setup()
  }, transform = redact_abs_path)
  expect_equal(
    cli::hash_file_sha1(".github/workflows/rhub.yaml"),
    wf_upd_hash
  )

  # workflow file is outdated, overwrite
  expect_snapshot({
    rhub_setup(overwrite = TRUE)
  }, transform = redact_abs_path)
  expect_equal(cli::hash_file_sha1(".github/workflows/rhub.yaml"), wf_hash)
})

test_that("setup_find_r_package", {
  withr::local_options(cli.ansi = FALSE)

  # we need to do this because we are wrapping text and also using
  # `transform` in `export_snapshot()`.
  withr::local_options(cli.width = Inf)

  pkg <- test_path("fixtures/pkg")
  withr::local_dir(pkg)
  expect_snapshot({
    setup_find_r_package()
  }, transform = redact_abs_path)

  unlink(file.path(tempdir(), "DESCRIPTION"))
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dir.create(tmp)
  withr::local_dir(tmp)
  expect_snapshot(error = TRUE, {
    setup_find_r_package()
  })
})

test_that("setup_find_git_root", {
  withr::local_options(cli.ansi = FALSE)

  # we need to do this because we are wrapping text and also using
  # `transform` in `export_snapshot()`.
  withr::local_options(cli.width = Inf)

  unlink(file.path(tempdir(), ".git"), recursive = TRUE)
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dir.create(tmp)
  withr::local_dir(tmp)
  expect_snapshot(error = TRUE, {
    setup_find_git_root()
  })

  dir.create(".git")
  expect_snapshot({
    setup_find_git_root()
  }, transform = redact_abs_path)
})
