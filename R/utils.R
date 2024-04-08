
pkg_error <- function(..., .data = NULL, .class = NULL, .envir = parent.frame(),
                      call. = TRUE) {
  .hide_from_trace <- TRUE
  cnd <- new_error(
    call. = call.,
    cli::format_error(
      .envir = .envir,
      c(
        ...
      )
    )
  )

  if (length(.data)) cnd[names(.data)] <- .data
  if (length(class)) class(cnd) <- c(.class, class(cnd))

  cnd
}

stop <- function(..., call. = TRUE, domain = NA) {
  .hide_from_trace <- TRUE
  args <- list(...)
  if (length(args) == 1L && inherits(args[[1L]], "condition")) {
    throw(
      add_class(args[[1]], c("rlib_error_3_1", "rlib_error"), "end"),
      frame = parent.frame()
    )
  } else {
    throw(new_error(..., call. = call., domain = domain))
  }
}

stopifnot <- function(...) {
  assert_that(..., env = parent.frame())
}

add_class <- function(obj, classes, where = c("start", "end")) {
  where <- match.arg(where)
  nc <- c(
    if (where == "start") classes,
    class(obj),
    if (where == "end") classes
  )
  class(obj) <- unique(nc)
  obj
}

zip <- function(x, y) {
  mapply(FUN = c, x, y, SIMPLIFY = FALSE, USE.NAMES = FALSE)
}

first_char <- function(x) {
  substr(x, 1, 1)
}

last_char <- function(x) {
  substr(x, nchar(x), nchar(x))
}

unquote <- function(x) {
  as.character(ifelse(
    first_char(x) == last_char(x) & first_char(x) %in% c("'", '"'),
    substr(x, 2L, nchar(x) - 1L),
    x
  ))
}

has_emoji <- function() {
  if (!cli::is_utf8_output()) return(FALSE)
  if (isTRUE(opt <- getOption("pkg.emoji"))) return(TRUE)
  if (identical(opt, FALSE)) return(FALSE)
  if (Sys.info()[["sysname"]] != "Darwin") return(FALSE)
  TRUE
}

parse_url <- function(url) {
  re_url <- paste0(
    "^(?<protocol>[a-zA-Z0-9]+)://",
    "(?:(?<username>[^@/:]+)(?::(?<password>[^@/]+))?@)?",
    "(?<host>[^/]+)",
    "(?<path>.*)$"            # don't worry about query params here...
  )

  mch <- re_match(url, re_url)

  if (is.na(mch[[1]])) {
    ssh_re_url <- "^git@(?<host>[^:]+):(?<path>.*)[.]git$"
    mch <- re_match(url, ssh_re_url)

    # try without the trailing .git as well
    if (is.na(mch[[1]])) {
      ssh_re_url2 <- "^git@(?<host>[^:]+):(?<path>.*)$"
      mch <- re_match(url, ssh_re_url2)
    }

    if (is.na(mch[[1]])) {
      cli::cli_abort("Invalid URL: {.url {url}}")
    }

    # Used for accessing the server's API
    mch$protocol <- "https"
  }

  mch[c("protocol", "host", "path")]
}

read_file <- function(path) {
  bin <- readBin(path, "raw", file.size(path))
  chr <- rawToChar(bin)
  if (is.na(iconv(chr, "UTF-8", "UTF-8"))) {
    throw(pkg_error("{.path {path}} is not UTF-8, giving up. :("))
  }
  Encoding(chr) <- "UTF-8"
  chr
}

ansi_align_width <- function(text) {
  if (length(text) == 0) return(text)
  width <- max(cli::ansi_nchar(text, type = "width"))
  cli::ansi_align(text, width = width)
}

random_id <- function() {
  r <- paste0(sample(c(letters, LETTERS, 0:9), 20, replace = TRUE), collapse = "")
  gsub(" ", "-", cli::hash_animal(r, n_adj = 1)$hash)
}

readline <- function(prompt) {
  base::readline(prompt)
}

is_interactive <- function() {
  opt <- getOption("rlib_interactive")
  if (isTRUE(opt)) {
    TRUE
  } else if (identical(opt, FALSE)) {
    FALSE
  } else if (tolower(getOption("knitr.in.progress", "false")) == "true") {
    FALSE
  } else if (tolower(getOption("rstudio.notebook.executing", "false")) == "true") {
    FALSE
  } else if (identical(Sys.getenv("TESTTHAT"), "true")) {
    FALSE
  } else {
    interactive()
  }
}

update <- function (original, new) {
  if (length(new)) {
    original[names(new)] <- new
  }
  original
}

get_maintainer_email <- function(path = ".") {
  path <- normalizePath(path)
  if (is_dir(path)) {
    if (!file.exists(file.path(path, "DESCRIPTION"))) {
      stop("No 'DESCRIPTION' file found")
    }
    parse_email(desc::desc_get_maintainer(path))
  } else {
    dir.create(tmp <- tempfile())
    files <- utils::untar(path, list = TRUE, tar = "internal")
    desc <- grep("^[^/]+/DESCRIPTION$", files, value = TRUE)
    if (length(desc) < 1) stop("No 'DESCRIPTION' file in package")
    utils::untar(path, desc, exdir = tmp, tar = "internal")
    parse_email(desc::desc_get_maintainer(file.path(tmp, desc)))
  }
}

is_dir <- function(x) {
  file.info(x)$isdir
}

#' @importFrom rematch re_match

parse_email <- function(x) {
  unname(
    re_match(pattern = "<(?<email>[^>]+)>", x)[, "email"]
  )
}
