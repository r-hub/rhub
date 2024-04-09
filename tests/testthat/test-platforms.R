test_that("get_platforms", {
  # empty cache
  rm(list = ls(the_cache), envir = the_cache)

  withr::local_envvar(
    RHUB_PLATFORMS_URL = http$url("/platforms.json"),
    RHUB_CONTAINERS_URL = http$url("/manifest.json")
  )

  plt <- get_platforms()
  plt[[1]] <- gsub("\r\n", "\n", plt[[1]], fixed = TRUE)
  plt[[2]] <- gsub("\r\n", "\n", plt[[2]], fixed = TRUE)
  expect_snapshot({
    cli::hash_obj_sha1(plt[[1]])
    cli::hash_obj_sha1(plt[[2]])
  })
})

test_that("rhub_platforms", {
  # empty cache
  rm(list = ls(the_cache), envir = the_cache)

  withr::local_envvar(
    RHUB_PLATFORMS_URL = http$url("/platforms.json"),
    RHUB_CONTAINERS_URL = http$url("/manifest.json")
  )

  expect_snapshot({
    rhub_platforms()
  })

  # if a platform refers to a container that does not exit
  # that's an error, but it is not an error here, here we test the printing
  withr::local_envvar(
    RHUB_PLATFORMS_URL = http$url("/platforms2.json"),
    RHUB_CONTAINERS_URL = http$url("/manifest.json")
  )
  library(pillar)
  expect_snapshot({
    rhub_platforms()[]
  })
})

test_that("format.rhub_platforms", {
  # empty cache
  rm(list = ls(the_cache), envir = the_cache)

  withr::local_envvar(
    RHUB_PLATFORMS_URL = http$url("/platforms.json"),
    RHUB_CONTAINERS_URL = http$url("/manifest.json")
  )
  plt <- rhub_platforms()

  # VM with a single R version
  plt$r_version[[1]] <- "R 4.4.0"

  # Container w/o an alias
  plt$aliases[[nrow(plt)]] <- character()
  expect_snapshot({
    print(plt)
  })
})

test_that("summary.rhub_platforms", {
  # empty cache
  rm(list = ls(the_cache), envir = the_cache)

  withr::local_envvar(
    RHUB_PLATFORMS_URL = http$url("/platforms.json"),
    RHUB_CONTAINERS_URL = http$url("/manifest.json")
  )
  plt <- rhub_platforms()
  expect_snapshot({
    summary(plt)
  })
})

test_that("select_platforms", {
  # empty cache
  rm(list = ls(the_cache), envir = the_cache)

  # error getting platforms
  withr::local_envvar(
    RHUB_PLATFORMS_URL = http$url("/bad-bad-bad-ooops"),
    RHUB_CONTAINERS_URL = http$url("/manifest.json")
  )

  expect_snapshot(error = TRUE, {
    select_platforms()
  })

  withr::local_envvar(
    RHUB_PLATFORMS_URL = http$url("/platforms.json"),
    RHUB_CONTAINERS_URL = http$url("/manifest.json")
  )

  # non-interactive sessions need explicit platforms
  withr::local_options(rlib_interactive = FALSE)
  expect_snapshot(error = TRUE, {
    select_platforms()
  })

  # non-interactive mode
  withr::local_options(rlib_interactive = FALSE)
  expect_snapshot({
    select_platforms(c("linux", "clang18"))
  })
  expect_snapshot(error = TRUE, {
    select_platforms(c("linux", "clang18", "thisisnotit"))
  })

  # interactive mode uses readline()
  withr::local_options(rlib_interactive = TRUE)

  mockery::stub(select_platforms, "readline", function(prompt) {
    cat(prompt)
    cat("   1, 3, 9\n")
    "   1, 3, 9"
  })
  expect_snapshot({
    select_platforms()
  })

  mockery::stub(select_platforms, "readline", function(prompt) {
    cat(prompt)
    cat("0\n")
    "0"
  })
  expect_snapshot(error = TRUE, {
    select_platforms()
  })

  mockery::stub(select_platforms, "readline", function(prompt) {
    cat(prompt)
    cat("10000\n")
    "10000"
  })
  expect_snapshot(error = TRUE, {
    select_platforms()
  })
})
