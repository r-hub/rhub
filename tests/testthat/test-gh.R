test_that("parse_gh_url", {
  expect_snapshot({
    parse_gh_url("https://github.com/r-lib/cli")
    parse_gh_url("http://github.com/r-lib/cli")
    parse_gh_url("https://my.private.gh/user/repo")
    parse_gh_url("http://my.private.gh/user/repo")
  })
})

test_that("gh_headers", {
  expect_snapshot({
    gh_headers("mytoken")
  })
})

test_that("gh_query_process_response", {
  resp <- readRDS(test_path("fixtures/gh-response.rds"))
  expect_snapshot({
    gh_query_process_response(resp)
  })
})

test_that("gh_rest_get, async_gh_rest_get", {
  called <- FALSE
  mockery::stub(
    gh_rest_get,
    "async_gh_rest_get",
    function(...) called <<- TRUE
  )

  gh_rest_get("https://github.com", "/repos/r-hub/rhub", "secret")
  expect_true(called)

  resp <- readRDS(test_path("fixtures/gh-response.rds"))
  mockery::stub(
    async_gh_rest_get,
    "http_get",
    function(...) async_constant(resp)
  )
  json <- synchronise(async_gh_rest_get(
    "https://api.github.com",
    "/repos/r-hub/rhub/actions/workflows",
    "secret"
  ))

  expect_snapshot(json)
})

test_that("gh_rest_post, async_gh_rest_post", {
  called <- FALSE
  mockery::stub(
    gh_rest_post,
    "async_gh_rest_post",
    function(...) called <<- TRUE
  )

  data <- "foobar"
  gh_rest_post(
    "https://api.github.com",
    "/repos/r-lib/ps/actions/workflows/rhub.yaml/dispatches",
    "secret",
    data
  )
  expect_true(called)

  resp <- readRDS(test_path("fixtures/gh-response-post.rds"))
  mockery::stub(
    async_gh_rest_post,
    "http_post",
    function(...) async_constant(resp)
  )
  json <- synchronise(async_gh_rest_post(
    "https://api.github.com",
    "/repos/r-lib/ps/actions/workflows/rhub.yaml/dispatches",
    "secret",
    data
  ))

  expect_snapshot(json)
})

test_that("gh_gql_get, async_gh_gql_get", {
  called <- FALSE
  mockery::stub(
    gh_gql_get,
    "async_gh_gql_get",
    function(...) called <<- TRUE
  )

  query <- "{
    repository(owner: \"r-hub\", name: \"rhub\") {
      pullRequest(number: 579) {
        headRefOid
      }
    }
  }
  "

  url <- parse_gh_url("https://github.com/r-lib/ps")
  gh_gql_get(url$graphql, query, "secret")
  expect_true(called)

  resp <- readRDS(test_path("fixtures/gh-response-gql.rds"))
  mockery::stub(
    async_gh_gql_get,
    "http_post",
    function(...) async_constant(resp)
  )
  json <- synchronise(async_gh_gql_get(url$graphql, query, "secret"))

  expect_snapshot(json)
})
