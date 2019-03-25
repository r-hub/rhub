
check_print <- function(self, private) {
  self$update()
  for (x in private$status_) check_print2(x)
  invisible(self)
}

#' @importFrom parsedate parse_iso_8601
#' @importFrom prettyunits pretty_ms
#' @importFrom crayon make_style red yellow

check_print2 <- function(x) {

  title_line(paste0(x$package, " ", x$version, ": ", toupper(x$status)))

  greyish <- make_style("darkgrey")

  submitted_time <- as.numeric(Sys.time() - parse_iso_8601(x$submitted), units = "secs")
  submitted <- if (submitted_time > 0) {
    paste(pretty_ms(submitted_time * 1000), "ago")
  } else {
    "just now"
  }

  build_time <- if (!is.null(x$build_time) && x$build_time != 0) {
    paste0(greyish("  Build time: "), pretty_ms(x$build_time), "\n")
  }

  cat(
    sep = "",
    greyish("  Build ID:   "), x$id, "\n",
    greyish("  Platform:   "), x$platform$description, "\n",
    greyish("  Submitted:  "), submitted, "\n",
    build_time %||% "",
    "\n"
  )

  ## If not done, then this is all we do
  if (is.null(build_time)) return(invisible(x))

  ## R CMD check error
  if (tolower(x$status) != "preperror" && tolower(x$status) != "aborted") {
    makeshift <- structure(
      list(
        package = x$package,
        version = x$version,
        rversion = x$platform$rversion,
        output = list(stdout = x$check_output, stderr = "", status = 0),
        platform = x$platform$name,
        notes = x$result$notes,
        warnings = x$result$warnings,
        errors = x$result$errors
      ),
      class = "rcmdcheck"
    )
    print(makeshift, header = FALSE)

  ## Or we never got to R CMD check
  } else {
    clog <- gsub("+R-HUB-R-HUB-R-HUB", "", fixed = TRUE, x$preperror_log)
    clog <- gsub("\necho >>>>>=========[^\n]*\n", "\n", clog)
    clog <- gsub(
      "\n>>>>>=======* (.+)\n",
      yellow(sep = "", "\n\n", symbol$line, " \\1\n\n"),
      clog,
      perl = TRUE
    )

    cat(red(paste0(symbol$pointer, " Build failed during preparation or aborted\n")))
    cat(greyish("\n[...]\n"))
    cat(greyish(clog))
    cat("\n")
  }

  invisible(x)
}
