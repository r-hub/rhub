
#' @importFrom crayon yellow red underline

report_system_error <- function(msg, status) {

  if (status$status == 0) return()

  if (status$stderr == "") {
    stop(
      msg, ", unknown error, standard output:\n",
      yellow(status$stdout),
      call. = FALSE
    )

  } else {
    stop(
      underline(yellow(paste0("\n", msg, ", standard output:\n\n"))),
      yellow(status$stdout), "\n",
      underline(red("Standard error:\n\n")), red(status$stderr),
      call. = FALSE
    )
  }
}

#' @importFrom httr status_code

report_error <- function(response) {
  if (status_code(response) < 300) {
    invisible(response)
  } else {
    call <- sys.call(-1)
    stop(create_condition(response, "error", call = call))
  }
}

#' @importFrom httr content

create_condition <- function(response,
                             class = c("error", "warning", "message"),
                             call) {

  class <- match.arg(class)

  message <- content(response)$message %||% "rhub error"

  structure(
    list(message = message, call = call),
    class = c("rhub_error", class, "condition")
  )
}
