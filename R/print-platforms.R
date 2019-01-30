
check_print_system <- function(self, private, ...) {
  if (is.null(private$status_)) {
    cat("Updating status...\n")
    self$update()
  }
  for (x in private$status_) check_print_system2(x)
  invisible(self)
}


check_print_system2 <- function(x) {

  cat("- ", x$platform$description, "\n")
  
  invisible(x)
}
