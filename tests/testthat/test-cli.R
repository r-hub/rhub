test_that("cli_status", {
  withr::local_options(cli.ansi = FALSE)
  expect_snapshot({
    pid <- cli_status("This is a status message")
    cli::cli_status_clear(pid, result = "clear")
  })

  expect_snapshot({
    pid <- cli_status("This is a status message")
    cli::cli_status_clear(pid, result = "failed")
  })
})
