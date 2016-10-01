
context("build")

test_that("build_package", {
  ## directory is packed up properly
  create_minimal_package(pkg <- tempfile())
  path <- build_package(pkg, tempfile())
  expect_true(is_pkg_tarball(path))

  ## tarball is not touched
  path2 <- build_package(path, tempfile())
  expect_true(is_pkg_tarball(path2))
  expect_equal(file.info(path)$size, file.info(path2)$size)

  ## Error is reported if R CMD build fails
  create_minimal_package(pkg <- tempfile())
  file.remove(file.path(pkg, "DESCRIPTION"))
  expect_error(build_package(pkg, tempfile()))
})
