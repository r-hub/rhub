
#' @export
#' @importFrom jsonlite fromJSON

platforms <- function() {
  json <- query("GET PLATFORMS", as = "text")
  fromJSON(json, simplifyDataFrame = TRUE)
}
