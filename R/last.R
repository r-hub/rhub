
#' The last rhub check of this R session
#'
#' `rhub` caches the id(s) of the last submission. This can be retrieved
#' with `last_check`.
#'
#' @return An rhub_check object.
#'
#' @export
#' @examples
#' \dontrun{
#' check("packagedir")
#' last_check()
#' last_check()$livelog()
#' }

last_check <- function() {
  package_data$last_handle
}
