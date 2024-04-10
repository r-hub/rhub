parse_gh_url <- function(url) {
  pcs <- parse_url(url)
  host <- pcs$host
  if (pcs$host == "github.com") {
    api <- paste0(pcs$protocol, "://api.github.com")
    graphql <- paste0(pcs$protocol, "://api.github.com/graphql")
  } else {
    api <- paste0(pcs$protocol, "://", pcs$host, "/api/v3")
    graphql <- paste0(pcs$protocol, "://", pcs$host, "/api/graphql")
  }
  cmps <- strsplit(pcs$path, "/", fixed = TRUE)[[1]]
  if (length(cmps) >= 1 && cmps[1] == "") cmps <- cmps[-1]
  if (length(cmps) < 2) cmps <- c(cmps, "", "")[1:2]
  cmps[2] <- sub("[.]git$", "", cmps[2])
  list(
    host = host,
    api = api,
    graphql = graphql,
    user = cmps[1],
    repo = cmps[2],
    slug = paste0(cmps[1], "/", cmps[2]),
    pat_url = paste0(pcs$protocol, "://", host, "/", cmps[1], "/", cmps[2])
  )
}

gh_headers <- function(token) {
  c(
    Accept = "application/vnd.github+json",
    Authorization = paste0("Bearer ", token)
  )
}

gh_query_process_response <- function(resp) {
  if (grepl("^application/json\\b", resp$type)) {
    resp$content <- jsonlite::fromJSON(
      rawToChar(resp$content),
      simplifyVector = FALSE
    )
  }
  resp$headers <- curl::parse_headers_list(resp$headers)
  resp
}

gh_rest_get <- function(host, endpoint, token) {
  synchronise(async_gh_rest_get(host, endpoint, token = token))
}

async_gh_rest_get <- function(host, endpoint, token) {
  url <- paste0(host, endpoint)
  headers <- gh_headers(token)
  http_get(url, headers = headers)$
    then(gh_query_process_response)
}

gh_rest_post <- function(host, endpoint, token, data) {
  synchronise(async_gh_rest_post(host, endpoint, token, data))
}

async_gh_rest_post <- function(host, endpoint, token, data) {
  url <- paste0(host, endpoint)
  headers <- gh_headers(token)
  http_post(url, data = data, headers = headers)$
    then(gh_query_process_response)
}

gh_gql_get <- function(host, query, token) {
  synchronise(async_gh_gql_get(host, query, token))
}

async_gh_gql_get <- function(host, query, token) {
  headers <- gh_headers(token)
  data <- jsonlite::toJSON(list(query = query), auto_unbox = TRUE)
  http_post(host, headers = headers, data = data)$
    then(gh_query_process_response)
}
