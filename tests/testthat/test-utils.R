# We don't use expect_snapshot because it adds an extra ! at the
# beginning of the error message
test_that("pkg_error", {
  err <- tryCatch(
    throw(pkg_error(
      "!" = "This is not good!",
      "i" = "You should not use {.code foo}, use {.code bar} instead.",
      .data = list(foo = 1:3),
      call. = FALSE
    )),
    error = function(e) e
  )
  expect_snapshot(err)
  expect_equal(err$foo, 1:3)
})

# We don't use expect_snapshot because it adds an extra ! at the
# beginning of the error message
test_that("stop", {
  err <- tryCatch(
    stop(pkg_error(
      "!" = "This is not good!",
      "i" = "You should not use {.code foo}, use {.code bar} instead.",
      call. = FALSE
    )),
    error = function(e) e
  )
  expect_snapshot(err)
})

test_that("stop with message", {
  err <- tryCatch(
    stop("Ooopsie daily!"),
    error = function(e) e
  )
  expect_snapshot(err)
})

test_that("stopifnot", {
  expect_snapshot(error = TRUE, {
    stopifnot(1 == 2)
  })
})

test_that("zip", {
  expect_snapshot({
    zip(character(), character())
    zip(letters[1:5], LETTERS[1:5])
    zip("1", letters[1:5])
  })
})

test_that("first_char", {
  expect_equal(first_char("foo"), "f")
  expect_equal(first_char("f"), "f")
  expect_equal(first_char(letters), letters)
  expect_equal(first_char(paste(letters, LETTERS)), letters)
  expect_equal(first_char(""), "")
  expect_equal(first_char(character()), character())
})

test_that("last_char", {
  expect_equal(last_char("foo"), "o")
  expect_equal(last_char("f"), "f")
  expect_equal(last_char(letters), letters)
  expect_equal(last_char(paste(letters, LETTERS)), LETTERS)
  expect_equal(last_char(""), "")
  expect_equal(last_char(character()), character())
})

test_that("unquote", {
  keep <- list(
    "foo",
    "'foo",
    "foo'",
    "\"foo'",
    "'foo\"",
    letters,
    paste0("'", letters),
    paste0(letters, "'"),
    character()
  )
  for (k in keep) expect_equal(unquote(k), k, info = k)

  expect_snapshot({
    unquote("'quoted'")
    unquote(c("'quoted'", "not", '"quoted"'))
  })
})

test_that("has_emoji", {
  mockery::stub(has_emoji, "cli::is_utf8_output", FALSE)
  expect_false(has_emoji())

  mockery::stub(has_emoji, "cli::is_utf8_output", TRUE)
  withr::local_options(pkg.emoji = TRUE)
  expect_true(has_emoji())

  withr::local_options(pkg.emoji = FALSE)
  expect_false(has_emoji())

  withr::local_options(pkg.emoji = NULL)
  mockery::stub(has_emoji, "Sys.info", list(sysname = "Darwin"))
  expect_true(has_emoji())

  mockery::stub(has_emoji, "Sys.info", list(sysname = "Linux"))
  expect_false(has_emoji())
})

test_that("parse_url", {
  expect_snapshot({
    parse_url("https://github.com/r-hub/rhub")
    parse_url("https://user@github.com/r-hub/rhub")
    parse_url("https://user:pass@github.com/r-hub/rhub")
    parse_url("https://github.com/r-hub/rhub?q=foo&p=bar")

    parse_url("git@github.com:/r-hub/rhub")
    parse_url("git@github.com:/r-hub/rhub.git")
  })

  expect_snapshot(error = TRUE, {
    parse_url("this is not a URL at all")
  })
})

test_that("read_file", {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)

  cnt <- as.raw(c(0xc3, 0xa9, 0xc3, 0xa1))
  writeBin(cnt, tmp)
  cnt2 <- read_file(tmp)
  expect_equal(Encoding(cnt2), "UTF-8")
  expect_equal(charToRaw(cnt2), cnt)

  writeBin(cnt[1:3], tmp)
  expect_error(read_file(tmp), "not UTF-8")
})

cli::test_that_cli("ansi_align_width", configs = c("plain", "ansi"), {
  expect_snapshot({
    paste0("--", ansi_align_width(c("foo", "bar", "foobar")), "--")
    paste0(
      "--",
      ansi_align_width(c("foo", "bar", cli::col_red("foobar"))),
      "--"
    )
    ansi_align_width(character())
  })
})

test_that("random_id", {
  expect_true(is.character(random_id()))
  expect_true(nchar(random_id()) >= 5)
})

test_that("readline", {
  args <- NULL
  mockery::stub(
    readline,
    "base::readline",
    function(...) args <<- list(...)
  )
  readline(prompt = "prompt")
  expect_equal(args, list("prompt"))
})

test_that("is_interactive", {
  withr::local_options(rlib_interactive = TRUE)
  expect_true(is_interactive())

  withr::local_options(rlib_interactive = FALSE)
  expect_false(is_interactive())

  withr::local_options(rlib_interactive = NULL)
  withr::local_options(knitr.in.progress = TRUE)
  expect_false(is_interactive())

  withr::local_options(knitr.in.progress = NULL)
  withr::local_options(rstudio.notebook.executing = TRUE)
  expect_false(is_interactive())

  withr::local_options(rstudio.notebook.executing = NULL)
  withr::local_envvar(TESTTHAT = "true")
  expect_false(is_interactive())

  withr::local_envvar(TESTTHAT = NA_character_)
  mockery::stub(is_interactive, "interactive", FALSE)
  expect_false(is_interactive())
  mockery::stub(is_interactive, "interactive", TRUE)
  expect_true(is_interactive())
})

test_that("update", {
  orig <- list(a = 1, b = 2)
  expect_equal(update(orig, list()), orig)
  expect_equal(update(orig, list(a = 2, c = 3)), list(a = 2, b = 2, c = 3))
})

test_that("get_maintainer_email", {
  pkg <- test_path("fixtures/pkg")
  expect_equal(get_maintainer_email(pkg), "Josiah.Carberry@example.com")

  pkg2 <- test_path("fixtures/pkg_0.0.0.9000.tar.gz")
  expect_equal(get_maintainer_email(pkg2), "first.last@example.com")

  bad <- tempfile()
  on.exit(unlink(bad, recursive = TRUE), add = TRUE)
  dir.create(bad)
  expect_error(get_maintainer_email(bad), "file found")

  bad2 <- test_path("fixtures/bad.tar.gz")
  expect_error(get_maintainer_email(bad2), "file in package")
})

test_that("is_dir", {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  file.create(tmp)
  expect_true(is_dir(tempdir()))
  expect_false(is_dir(tmp))
})
