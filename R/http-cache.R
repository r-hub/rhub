
the_cache <- new.env(parent = emptyenv())

async_cached_http_get <- function(url, headers = character(),
                                  options = list()) {
  hash <- cli::hash_md5(paste0("http-get-", url))
  if (hash %in% names(the_cache)) {
    async_constant(the_cache[[hash]])
  } else {
    http_get(url, headers = headers, options = options)$
      then(http_stop_for_status)$
      then(function(response) {
        json <- rawToChar(response$content)
        the_cache[[hash]] <- json
        json
      })
  }
}
