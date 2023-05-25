
test_that("parse_gh_url", {
  expect_snapshot({
    parse_gh_url("https://github.com/foo/bar.git")
    parse_gh_url("https://myserver.org/foo/bar.git")
  })
})
