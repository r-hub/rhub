test_that("query GET", {
  withr::local_envvar(RHUB_SERVER = http$url())
  expect_snapshot({
    cat(rawToChar(query("/get")$content))
  }, transform = function(x) redact_port(redact_ae_header(x)))
})

test_that("query HTTP errors", {
  withr::local_envvar(RHUB_SERVER = http$url())
  expect_snapshot(error = TRUE, {
    query("/rhub-error?msg=iamsosorryabouththat")
  })
  expect_snapshot(error = TRUE, {
    query("/rhub-error2")
  })
  expect_snapshot(error = TRUE, {
    query("/rhub-error3")
  })
})

test_that("query POST", {
  withr::local_envvar(RHUB_SERVER = http$url())
  data <- charToRaw(jsonlite::toJSON(list(foo = "bar", foobar = 1:3)))
  expect_snapshot({
    cat(rawToChar(query("/post", method = "POST", data = data)$content))
  }, transform = function(x) redact_port(redact_ae_header(x)))
})

test_that("query, unknown verb", {
  withr::local_envvar(RHUB_SERVER = http$url())
  expect_snapshot(error = TRUE, {
    query("/anything", method = "REPORT")
    query("/anything", method = "REPORT", sse = TRUE)
  }, transform = redact_port)
})

test_that("query SSE", {
  withr::local_envvar(RHUB_SERVER = http$url())
  data <- charToRaw(jsonlite::toJSON(list(foo = "bar", foobar = 1:3)))
  expect_snapshot({
    query("/sse", sse = TRUE)$sse
    query("/sse", method = "POST", data = data, sse = TRUE)$sse
  })

  # progress, result
  expect_snapshot({
    resp <- query("/sse?progress=true&numevents=2", sse = TRUE)
    cat(rawToChar(resp$content))
  })

  # progress, result, error
  expect_snapshot(error = TRUE, {
    resp <- query("/sse?progress=true&numevents=2&error=true", sse = TRUE)
    cat(rawToChar(resp$content))
  })

})
