
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
      add_class(args[[1]], c("rlib_error_3_0", "rlib_error"), "end"),
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
  mapply(FUN = c, x, y, SIMPLIFY = FALSE)
}

first_char <- function(x) {
  substr(x, 1, 1)
}

last_char <- function(x) {
  substr(x, nchar(x), nchar(x))
}

unquote <- function(x) {
  ifelse(
    first_char(x) == last_char(x) & first_char(x) %in% c("'", '"'),
    substr(x, 2L, nchar(x) - 1L),
    x
  )
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
  mch[, setdiff(colnames(mch), c(".match", ".text")), drop = FALSE]
}

read_file <- function(path) {
  bin <- readBin(path, "raw", file.size(path))
  chr <- rawToChar(bin)
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
