
context("print")

test_that("header_line", {
  expect_output(
    header_line("title"),
    "title"
  )
})
