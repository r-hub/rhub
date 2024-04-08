test_that("is_character", {
  expect_snapshot({
    is_character(character())
    is_character("a")
    is_character(c("a", "b", "c"))
  })

  expect_snapshot(error = TRUE, {
    x <- 1
    assert_that(is_character(x))
    x <- mtcars
    assert_that(is_character(x))
    x <- NULL
    assert_that(is_character(x))
    x <- c("a", "b", NA_character_)
    assert_that(is_character(x))
  })
})

test_that("is_optional_character", {
  expect_snapshot({
    is_optional_character(NULL)
    is_optional_character(character())
    is_optional_character("a")
    is_optional_character(c("a", "b", "c"))
  })

  expect_snapshot(error = TRUE, {
    x <- 1
    assert_that(is_optional_character(x))
    x <- mtcars
    assert_that(is_optional_character(x))
    x <- c("a", "b", NA_character_)
    assert_that(is_optional_character(x))
  })
})

test_that("is_string", {
  expect_snapshot({
    is_string("a")
  })

  expect_snapshot(error = TRUE, {
    x <- 1
    assert_that(is_string(x))
    x <- mtcars
    assert_that(is_string(x))
    x <- NULL
    assert_that(is_string(x))
    x <- NA_character_
    assert_that(is_string(x))
    x <- c("a", "b", NA_character_)
    assert_that(is_string(x))
    x <- character()
    assert_that(is_string(x))
    x <- c("a", "b")
    assert_that(is_string(x))
  })
})

test_that("is_optional_string", {
  expect_snapshot({
    is_optional_string("a")
    is_optional_string(NULL)
  })

  expect_snapshot(error = TRUE, {
    x <- 1
    assert_that(is_optional_string(x))
    x <- mtcars
    assert_that(is_optional_string(x))
    x <- NA_character_
    assert_that(is_optional_string(x))
    x <- c("a", "b", NA_character_)
    assert_that(is_optional_string(x))
    x <- character()
    assert_that(is_optional_string(x))
    x <- c("a", "b")
    assert_that(is_optional_string(x))
  })
})

test_that("is_optional_gh_url", {
  expect_snapshot({
    is_optional_gh_url(NULL)
    is_optional_gh_url("https://github.com")
    is_optional_gh_url("http://github.com")
  })

  expect_snapshot(error = TRUE, {
    gh_url <- 1:10
    assert_that(is_optional_gh_url(gh_url))

    gh_url <- "foobar"
    assert_that(is_optional_gh_url(gh_url))
  })
})