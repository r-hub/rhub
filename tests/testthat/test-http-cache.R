test_that("async_cached_http_get", {
  # empty cache
  rm(list = ls(the_cache), envir = the_cache)

  resp <- synchronise(async_cached_http_get(http$url("/get")))
  expect_equal(length(the_cache), 1)
  expect_equal(resp, get(ls(the_cache), envir = the_cache))

  # will not perform HTTP request now
  mockery::stub(async_cached_http_get, "http_get", function(...) stop("no"))
  resp2 <- synchronise(async_cached_http_get(http$url("/get")))
  expect_equal(length(the_cache), 1)
  expect_equal(resp2, get(ls(the_cache), envir = the_cache))
})