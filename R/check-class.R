
#' An `rhub_check` object holds status and results of rhub checks
#'
#' @section Usage:
#' ```
#' ch$update()
#' ch$print(...)
#' ch$browse(which = NULL)
#' ch$livelog(which = 1)
#' ```
#'
#' @section Arguments:
#' * `ch` An rhub check object. It can be created using [`check()`],
#'   and other check functions including [`check_for_cran`].
#'   See also [last_check()].
#' * `...` Extra arguments are currently ignored.
#' * `which` Which check to show, if the object contains multiple
#'   checks. For `browse` the default is all checks. For `livelog` the
#'   default is the first check. A check can be selected via its number
#'   or id.
#'
#' @section Details:
#'
#' An `rhub_check` object can be created by [check()], [list_my_checks()],
#' or [list_package_checks()]. [last_check()] returns the last check(s)
#' submitted from the current R session. Do not confuse `rhub_check`/`rhub_check_for_cran` 
#' (classes) with [check()] or [check_for_cran()] (functions).
#'
#'
#' `ch$update()` updates the status of the check. Printing the check
#' status to the screen does not perform an update, unless the status of
#' the check(s) is unknown.
#'
#' `ch$print()` prints the status of the check(s) to the screen.
#' 
#' `ch$cran_summary()` prints text to be copy-pasted in cran-comments.md,
#'  it is especially useful on the output of [`check_for_cran()`].
#'
#' `ch$browse()` opens a tab or window in the default web browser, that points
#' to the detailed logs of the check(s).
#' 
#' `ch$urls()` return a table with URL to the html log, text log and artifacts
#' of the check(s).
#' 
#' For both `ch$browse()` and `ch$urls()`, not that the logs and artifacts 
#' are not kept forever, they are accessible for a few days after submission.
#'
#' `ch$livelog()` shows the live log of the check. The live log can be
#' interrupted using the usual interruption keyboard shortcut, usually
#' `CTRL+c` or `ESC`.
#' 
#' @name rhub_check
#' @examples
#' \dontrun{
#' check()
#' ch <- last_check()
#' ch$update()
#' ch$browse()
#' ch$livelog()
#' }
NULL

#' @importFrom R6 R6Class
#' @export

rhub_check <- R6Class(
  "rhub_check",

  public = list(

    initialize = function(ids, status = NULL)
      check_init(self, private, ids, status),

    update = function()
      check_update(self, private),

    print = function(...)
      check_print(self, private, ...),

    web = function(which = NULL)
      check_web(self, private, which),
    
    browse = function(which = NULL)
      self$web(which),
    
    urls = function(which = NULL)
      check_urls(self, private, which),

    livelog = function(which = 1)
      check_livelog(self, private, which),
    
    cran_summary = function(...)
      check_cran_summary(self, private, ...)
  ),

  private = list(
    ids_ = NULL,                        # character vector of ids
    status_ = NULL,                     # list of status objects, as in DB
    status_updated_ = NULL              # last time status was updated
  )
)

check_init <- function(self, private, ids, status) {
  assert_that(is_check_ids(ids))
  private$ids_ <- ids
  private$status_ <- status
  status_updated_ <- Sys.time()
  invisible(self)
}

check_update <- function(self, private) {
  private$status_ <- query("GET STATUS", data = list(id = private$ids_))
  invisible(self)
}

#' @importFrom utils browseURL

check_web <- function(self, private, which) {

  ids <- select_ids(which = which, self = self, 
                    private = private)

  urls <- paste0(sub("/api$", "/status/", baseurl()), ids)
  
  lapply(urls, browseURL)
  invisible(self)
}

check_urls <- function(self, private, which) {
  
  ids <- select_ids(which = which, self = self, 
                    private = private)
  
  data.frame(html = paste0(sub("/api$", "/status/", baseurl()), ids),
             text = paste0(sub("/api$", "/status/original/", baseurl()), 
                           ids),
             artifacts = paste0("https://artifacts.r-hub.io/", ids),
             stringsAsFactors = FALSE)
}

select_ids <- function(which, self, private){
  ids <- if (is.null(which)) {
    private$ids_
  } else if (is.numeric(which)) {
    private$ids_[which]
  } else if (is.character(which)) {
    intersect(private$ids_, which)
  } else {
    stop("Unknown check selected",
         call. = FALSE)
  }
  
  return(ids)
}

check_cran_summary <- function(self, private, ...) {
  
  if (is.null(private$status_)) {
    cat("Updating status...\n")
    self$update()
  }
  
  x <- private$status_
  
  cat("## Test environments\n")
  result <- do.call("rbind",
                    lapply(x, rectangle_status))
  systems <- paste0(vapply(x, function(xx) xx$platform$name, ""),
                    " (",
                    vapply(x, function(xx) xx$platform$rversion, ""),
                    ")")
  cat(paste0("- R-hub ",
             systems,
             "\n"))
  cat("\n")
  cat("## R CMD check results\n")
  
  unique_results <- unique(result[, c("type", "hash")])
  
  makeshift <- structure(
    list(
      package = x$package,
      version = toString(vapply(x, function(xx) xx$platform$name, "")),
      rversion = toString(systems),
      output = list(),
      platform = toString(systems),
      notes = unlist(lapply(unique(unique_results$hash[unique_results$type == "NOTE"]),
                            combine_message, result = result)),
      warnings = unlist(lapply(unique(unique_results$hash[unique_results$type == "WARNING"]),
                               combine_message, result = result)),
      errors = unlist(lapply(unique(unique_results$hash[unique_results$type == "ERROR"]),
                             combine_message, result = result))
    ),
    class = "rcmdcheck"
  )
  print(makeshift, header = FALSE)
  
  invisible(self)
}


get_status_part <- function(part, x){
  output <- unlist(x[part])
  if(is.null(output)){
    return("")
  }else{
    output
  }
}

rectangle_status <- function(x){
  df <- rbind(data.frame(type = "ERROR",
                         message = get_status_part("errors", x$result),
                         stringsAsFactors = FALSE),
              data.frame(type = "WARNING",
                         message = get_status_part("warnings", x$result),
                         stringsAsFactors = FALSE),
              data.frame(type = "NOTE",
                         message = get_status_part("notes", x$result),
                         stringsAsFactors = FALSE))
  
  df <- df[df$message != "",]
  df$package <- x$package
  df$version <- x$version
  df$submitted <- x$submitted
  df$platform <- paste0(x$platform$name, " (", x$platform$rversion,
                        ")")
  df$hash <- hash_check(df$message)
  
  return(df)
}

combine_message <- function(hash, result){
  paste0("On ", toString(result$platform[result$hash == hash]), "\n",
         result$message[result$hash == hash][1])
}

# from rcmdcheck https://github.com/r-lib/rcmdcheck/blob/968fd9ba76ee9b7bf65d192568555ab57160165e/R/parse.R#L110
#' @importFrom digest digest

hash_check <- function(check) {
  cleancheck <- gsub("[^a-zA-Z0-9]", "", first_line(check))
  vapply(cleancheck, digest::digest, "")
}

# from rcmdcheck https://github.com/r-lib/rcmdcheck/blob/afadc6c53310cad2b64e0a58e399efd1ae18d7dd/R/utils.R#L91
first_line <- function(x) {
  l <- strsplit(x, "\n", fixed = TRUE)
  vapply(l, "[[", "", 1)
}
