
pkg_error <- function(..., .data = NULL, .class = NULL, .envir = parent.frame(),
                      call. = TRUE) {
  .hide_from_trace <- TRUE
  cnd <- new_error(
    call. = call.,
    cli::format_error(
      .envir = .envir,
      c(
        ...
      )
    )
  )

  if (length(.data)) cnd[names(.data)] <- .data
  if (length(class)) class(cnd) <- c(.class, class(cnd))

  cnd
}

stop <- function(..., call. = TRUE, domain = NA) {
  .hide_from_trace <- TRUE
  args <- list(...)
  if (length(args) == 1L && inherits(args[[1L]], "condition")) {
    throw(
      add_class(args[[1]], c("rlib_error_3_0", "rlib_error"), "end"),
      frame = parent.frame()
    )
  } else {
    throw(new_error(..., call. = call., domain = domain))
  }
}

stopifnot <- function(...) {
  assert_that(..., env = parent.frame())
}

add_class <- function(obj, classes, where = c("start", "end")) {
  where <- match.arg(where)
  nc <- c(
    if (where == "start") classes,
    class(obj),
    if (where == "end") classes
  )
  class(obj) <- unique(nc)
  obj
}
