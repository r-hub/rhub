test_that("cli_status", {
  expect_snapshot({
    pid <- cli_status("This is a status message")
    cli::cli_status_clear(pid, result = "clear")
  })

  expect_snapshot({
    pid <- cli_status("This is a status message")
    cli::cli_status_clear(pid, result = "failed")
  })
})
