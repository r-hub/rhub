
context("api")

test_that("query", {
  res <- NULL
  with_mock(
    `httr::GET` = function(...) res <<- list(...),
    `httr::status_code` = function(...) { 200 },
    `httr::headers` = function(...) { },
    `httr::content` = function(...) { },
    query("GET PLATFORMS")
  )
  expect_equal(length(res), 3)
  expect_true(is_string(res[[1]]))
  expect_equal(class(res[[2]]), "request")

  called <- FALSE
  with_mock(
    `rhub:::get_endpoint` = function(endpoint, params)
      list(method = endpoint, path = "p"),
    `httr::POST` = function(...) called <<- "POST",
    `httr::DELETE` = function(...) called <<- "DELETE",
    `rhub:::report_error` = function(...) { },
    `rhub:::parse_response` = function(...) { },
    query("POST"),
    expect_identical(called, "POST"),
    query("DELETE"),
    expect_identical(called, "DELETE"),
    expect_error(query("FOOBAR"), "Unexpected HTTP verb")
  )
})

test_that("parse_response", {
  with_mock(
    `httr::headers` = function(...)
      list("content-type" = "application/json; charset: utf8"),
    `httr::content` = function(...)
      '{ "foo": "bar", "bar": [1,2,3] }',
    expect_equal(
      parse_response(NULL, as = NULL),
      list(foo = "bar", bar = list(1,2,3))
    ),
    expect_equal(
      parse_response(NULL, as = "text"),
      '{ "foo": "bar", "bar": [1,2,3] }'
    )
  )

  with_mock(
    `httr::headers` = function(...) list(),
    `httr::content` = function(...) "foobar",
    expect_equal(parse_response(NULL), "foobar")
  )

  with_mock(
    `httr::headers` = function(...) list("content-type" = "text/plain"),
    `httr::content` = function(...) "foobar",
    expect_equal(parse_response(NULL), "foobar")
  )
})
