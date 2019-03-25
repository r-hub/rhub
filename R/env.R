
package_data <- new.env(parent = emptyenv())
package_data$status <- new.env(parent = emptyenv())
package_data$ids <- character()
package_data$groups <- character()

## Since the status can be NULL, meaning unknown, we put all cache elements
## in a list of length 1.

cache_get <- function(id) {
  e <- package_data$status
  if (!is.null(x <- e[[id]][[1]])) return(x)
  nms <- ls(e)
  sts <- grep(paste0("-", id, "[0-9a-f]*$"), nms)
  if (length(sts) == 0) return(NULL)
  if (length(sts) == 1) return(e[[ nms[sts] ]][[1]])
  stop("Multiple checks match, please use a more specific id", call. = FALSE)
}

cache_put <- function(id, status) {
  cache_put_ids(id)
  cache_put_group_ids(status$group)
  package_data$status[[id]] <- list(status)
  invisible()
}

cache_put_ids <- function(id) {
  id <- unique(setdiff(id, package_data$ids))
  if (length(id)) package_data$ids <- c(id, package_data$ids)
}

cache_put_group_ids <- function(id) {
  id <- unique(setdiff(id, package_data$groups))
  if (length(id)) package_data$groups <- c(id, package_data$groups)
}

cache_get_ids <- function(ids) {
  w <- match_partial(ids, package_data$ids)
  package_data$ids[w]
}

cache_get_group_ids <- function(ids) {
  w <- match_partial(ids, package_data$groups)
  package_data$groups[w]
}

match_partial <- function(x, table) {
  hash <- sub("^.*-", "", table)
  m <- match(x, table)
  ifelse(is.na(m), pmatch(x, hash), m)
}
