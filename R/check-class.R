
#' An `rhub_check` object holds status and results of rhub checks
#'
#' @section Usage:
#' ```
#' ch$update()
#' ch$print(...)
#' ch$web(which = NULL)
#' ch$livelog(which = 1)
#' ```
#'
#' @section Arguments:
#' * `ch` An rhub check object. It can be created using [check()],
#'   [list_my_checks()] or [list_package_checks()].
#'   See also [last_check()].
#' * `...` Extra arguments are currently ignored.
#' * `which` Which check to show, if the object contains multiple
#'   checks. For `web` the default is all checks. For `livelog` the
#'   default is the first check. A check can be selected via its number
#'   or id.
#'
#' @section Details:
#'
#' An `rhub_check` object can be created by [check()], [list_my_checks()],
#' or [list_package_checks()]. [last_check()] returns the last check(s)
#' submitted from the current R session.
#'
#' `ch$update()` updates the status of the check. Printing the check
#' status to the screen does not perform an update, unless the status of
#' the check(s) is unknown.
#'
#' `ch$print()` prints the status of the check(s) to the screen.
#'
#' `ch$web()` opens a tab or window in the default web browser, that points
#' to the detailed logs of the check.
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
#' ch$web()
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

    livelog = function(which = 1)
      check_livelog(self, private, which)
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

  ids <- if (is.null(which)) {
    private$ids_
  } else if (is.numeric(which)) {
    private$ids_[which]
  } else if (is.character(which)) {
    intersect(private$ids_, which)
  } else {
    stop("Unknown check selected")
  }

  urls <- paste0(sub("/api$", "/status/", baseurl), ids)
  lapply(urls, browseURL)
  invisible(self)
}
