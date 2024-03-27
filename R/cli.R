
cli_status <- function(msg, ..., .auto_close = FALSE) {
  msg
  cli::cli_status(
    msg = "{.alert {msg}}",
    msg_done = "{.alert-success {msg}}",
    msg_failed = "{.alert-danger {msg}}",
    .auto_close = .auto_close,
    ...
  )
}
