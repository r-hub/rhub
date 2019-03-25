
column_dt <- function(x) {
  as.difftime(x / 1000, units = "secs")
}

#' @importFrom pillar pillar_shaft new_pillar_shaft_simple
#' @importFrom prettyunits pretty_dt
#' @export

pillar_shaft.difftime <- function(x, ...) {
  cx <- my_pretty_dt(x)
  new_pillar_shaft_simple(cx, ...)
}
