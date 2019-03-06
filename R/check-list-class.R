
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
#' and `chl$livelog()` methods.
#'
#' `chl$details()` prints the details for all checks in the list.
#' 
#' `chl$summary()` gives a `data.frame` with a line per check indicating `package`,
#' `version`, `status`, `submitted`, `platform`, `group_id` if the check
#'  is part of a group of checks sent simultaneously to the builder (e.g. via
#'  [`check_for_cran()`], `check_id`.
#'  
#'  `chl$print()` prints this `data.frame`.
#'  
#'  `chl$get_group(group_id)` returns a [`rhub_check`] for checks of the 
#'  list sharing the `group_id`.
#'  
#'  `chl$get_check(check_id)` returns a [`rhub_check`] for the check of the 
#'  list that has the `check_id`.
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

rhub_check_list <- R6Class(
  "rhub_check_list",
  inherit = rhub_check,

  public = list(
    details = function()
      check_list_details(self, private, super),

    print = function(...)
      check_list_print(self, private, ...),
    
    summary = function(...)
      check_list_summary(self, private, ...),
    
    get_check = function(check_id, ...)
      check_list_get_check(check_id, self, private, ...),
    
    get_group = function(group_id, ...)
      check_list_get_group(group_id, self, private, ...)
  )
)

check_list_details <- function(self, private, super) {
  super$print()
}

check_list_summary <- function(self, private, ...) {
  
  if (is.null(private$status_)) {
    cat("Updating status...\n")
    self$update()
  }
  
  x <- private$status_
  
  package <- vapply(x, "[[", "", "package")
  version <- vapply(x, "[[", "", "version")
  status <- vapply(x, "[[", "", "status")
  submitted <- vapply(x, "[[", "", "submitted")
  check_id <- vapply(x, "[[", "", "id")
  group_id <- vapply(x, get_group, "")
  group_id[group_id == ""] <- NA
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
  
  data_frame(
    package = package,
    version = version,
    status = status,
    submitted = submitted,
    platform = platform,
    group_id = group_id,
    check_id = check_id
  )
  
}

check_list_print <- function(self, private, ...) {

  summary <- self$summary()
  
  print(summary,
  row.names = FALSE)

  invisible(self)
}

check_list_get_group <- function(group_id, self, private){
  summary <- self$summary()
  
  if (! group_id %in% summary$group_id){
    stop("This is not a valid group_id for this check list", call. = FALSE)
  }
  
  ids <- summary$check_id[!is.na(summary$group_id) &
                            summary$group_id == group_id]
  
  rhub_check$new(ids)
}

check_list_get_check <- function(check_id, self, private){
  summary <- self$summary()
  
  if (! check_id %in% summary$check_id){
    stop("This is not a valid check_id for this check list", call. = FALSE)
  }
  
  rhub_check$new(check_id)
}
