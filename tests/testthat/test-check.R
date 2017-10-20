
context("check")

test_that("check", {
  pkg <- create_minimal_package()

  ## From tarball
  pkg_targz <- build_package(pkg, tempfile())
  sub <- NULL
  ch <- with_mock(
    `rhub::assert_validated_email_for_check` = function(...) TRUE,
    `rhub::submit_package` = function(...) {
      sub <<- list(...)
      list(list(id = "foobar"))
    },
    `rhub:::match_platform` = function(x) x,
    check(pkg_targz, email = "e", platform = "p", show_status = FALSE)
  )

  expect_equal(sub[[1]], "e")
  expect_equal(sub[[3]], "p")
  expect_equal(sub[[4]], character())
})

test_that("check shortcuts", {
  with_mock(
    `rhub::check` = function(path = ".", platform, ...) platform,
    expect_equal(check_on_linux(), check_shortcut_platforms$linux),
    expect_equal(check_on_windows(), check_shortcut_platforms$windows),

    expect_equal(check_on_debian(), check_shortcut_platforms$debian),
    expect_equal(check_on_ubuntu(), check_shortcut_platforms$ubuntu),
    expect_equal(check_on_fedora(), check_shortcut_platforms$fedora),

    expect_equal(check_with_roldrel(), check_shortcut_platforms$roldrel),
    expect_equal(check_with_rrelease(), check_shortcut_platforms$rrelease),
    expect_equal(check_with_rpatched(), check_shortcut_platforms$rpatched),
    expect_equal(check_with_rdevel(), check_shortcut_platforms$rdevel),

    expect_equal(check_with_valgrind(), check_shortcut_platforms$valgrind),
    expect_equal(
      check_with_sanitizers(),
      check_shortcut_platforms$sanitizers
    )
  )
})
