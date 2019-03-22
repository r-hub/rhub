
package_data <- new.env(parent = emptyenv())
package_data$status <- new.env(parent = emptyenv())

## Since the status can be NULL, meaning unknown, we put all cache elements
## in a list of length 1.

cache_get <- function(id) {
  e <- package_data$status
  if (!is.null(x <- e[[id]][[1]])) return(x)
  nms <- ls(e)
  sts <- grep(paste0("-", id, "[0-9a-f]*$"), nms)
  if (length(sts) == 0) return(NULL)
  if (length(sts) == 1) return(e[[ nms[sts] ]][[1]])
  stop("Multiple builds match, please use a more specific id", call. = FALSE)
}

cache_put <- function(id, status) {
  package_data$status[[id]] <- list(status)
  invisible()
}
