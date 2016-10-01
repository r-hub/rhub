
context("error")

test_that("report_system_error", {
  expect_silent(report_system_error(status = list(status = 0)))

  expect_error(
    report_system_error(
      "this is unacceptable",
      list(status = 1, stderr = "", stdout = "out")
    ),
    "this is unacceptable"
  )

  expect_error(
    report_system_error(
      "this is unacceptable",
      list(status = 1, stderr = "puff", stdout = "out")
    ),
    "this is unacceptable.*puff"
  )
})

test_that("report_error", {
  with_mock(
    `httr::status_code` = function(response) 200,
    expect_silent(report_error("dummy"))
  )
  with_mock(
    `httr::status_code` = function(response) 404,
    expect_error(report_error("dummy"))
  )
})

test_that("create_condition", {
  with_mock(
    `httr::content` = function(...) list(message = "not at all"),
    cond <- create_condition("not good", call = sys.call())
  )
  expect_true("rhub_error" %in% class(cond))
})
