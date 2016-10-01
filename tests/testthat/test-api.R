
context("api")

test_that("query", {
  res <- NULL
  with_mock(
    `httr::GET` = function(...) res <<- list(...),
    `httr::status_code` = function(...) { 200 },
    `httr::headers` = function(...) { },
    query("GET PLATFORMS")
  )
  expect_equal(length(res), 2)
  expect_silent(assert_string(res[[1]]))
  expect_equal(class(res[[2]]), "request")
})

test_that("get_endpoint", {
  ## TODO
})

test_that("parse_response", {
  ## TODO
})
