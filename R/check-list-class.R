
#' An `rhub_check_list` object, a list of rhub check results
#'
#' This class is similar to [rhub_check] (it inherits from it),
#' but it shows the results of the checks in a more concise format
#' by default. To show the details, call the `$details()` method.
#'
#' @section Usage:
#' ```
#' chl$update()
#' chl$print(...)
#' chl$details()
#' chl$livelog(which = 1)
#'
#' @section Details:
#'
#' The [list_my_checks()] and [list_package_checks()] functions create
#' `rhub_check_list` objects.
#'
#' See [rhub_check] for the description of the inherited `chl$update()`,
#' `chl$print()`, `chl$livelog()` methods.
#'
#' `chl$details()` prints the details for all checks in the list.
#'
#' @name rhub_check_list
#' @rdname rhub_check
#' @examples
#' \dontrun{
#' chl <- list_my_checks()
#' chl
#' chl$details()
#' }
NULL

#' @export

rhub_check_list <- R6Class(
  "rhub_check_list",
  inherit = rhub_check,

  public = list(
    details = function()
      check_list_details(self, private, super),

    print = function(...)
      check_list_print(self, private, ...)
    )
)

check_list_details <- function(self, private, super) {
  super$print()
}

check_list_print <- function(self, private, ...) {

  if (is.null(private$status_)) {
    cat("Updating status...\n")
    self$update()
  }

  x <- private$status_

  package <- vapply(x, "[[", "", "package")
  version <- vapply(x, "[[", "", "version")
  status <- vapply(x, "[[", "", "status")
  submitted <- vapply(x, "[[", "", "submitted")
  platform <- vapply(x, function(xx) xx$platform$name, "")

  submitted <- if (length(package) == 0) {
    character()
  } else {
    tdiff <- Sys.time() - parse_iso_8601(submitted)
    units(tdiff) <- "secs"
    paste(
      pretty_ms(as.numeric(tdiff) * 1000, compact = TRUE),
      "ago"
    )
  }

  print(data_frame(
    package = package,
    version = version,
    status = status,
    submitted = submitted,
    platform = platform
  ))

  invisible(self)
}

check_list_report <- function(self, private, ...) {
  
  if (is.null(private$status_)) {
    cat("Updating status...\n")
    self$update()
  }
  
  x <- private$status_
  
  result <- do.call("rbind",
                    lapply(x, rectangle_status))
  systems <- paste0(vapply(x, function(xx) xx$platform$name, ""), 
  " (",
  vapply(x, function(xx) xx$platform$rversion, ""),
  ")")
  cat(paste0("- ", 
       systems,
       "\n"))
  
  unique_results <- unique(result[, c("type", "message")])
  unique_results <- unique_results[order(unique_results$message),]
  # TODO use rcmdcheck stuff
  makeshift <- structure(
    list(
      package = x$package,
      version = toString(vapply(x, function(xx) xx$platform$name, "")),
      rversion = toString(systems),
      output = list(),
      platform = toString(systems),
      notes = unlist(lapply(unique(unique_results$message[unique_results$type == "NOTE"]),
                     combine_message, result = result)),
      warnings = unlist(lapply(unique(unique_results$message[unique_results$type == "WARNING"]),
                               combine_message, result = result)),
      errors = unlist(lapply(unique(unique_results$message[unique_results$type == "ERROR"]),
                             combine_message, result = result))
    ),
    class = "rcmdcheck"
  )
  print(makeshift, header = FALSE)
  
  invisible(self)
}

## -----------------------------------------------------------------------

#' @name rhub_check_for_cran
#' @rdname rhub_check
NULL

#' @export

rhub_check_for_cran <- R6Class(
  "rhub_check_for_cran",
  inherit = rhub_check_list,

  public = list(
    report = function(...)
        check_list_report(self, private, ...)
  )
)

get_status_part <- function(part, x){
  output <- unlist(x[part])
  if(is.null(output)){
    return("")
  }else{
    output
  }
}

rectangle_status <- function(x){
  
  df <- rbind(tibble::tibble(type = "ERROR",
                       message = get_status_part("errors", x$result)),
        tibble::tibble(type = "WARNING",
                       message = get_status_part("warnings", x$result)),
        tibble::tibble(type = "NOTE",
                       message = get_status_part("notes", x$result)))
  
  
  df$package <- x$package
  df$version <- x$version
  df$submitted <- x$submitted
  df$platform <- paste0(x$platform$name, " (", x$platform$rversion,
                        ")")
  
  df[df$message != "",]
  
}

combine_message <- function(message, result){
  paste0("On ", toString(result$platform[result$message == message]), "\n",
         message)
}