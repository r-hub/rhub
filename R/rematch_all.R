
re_match_all <- function(text, pattern, ...) {

  text <- as.character(text)
  stopifnot(is.character(pattern), length(pattern) == 1, !is.na(pattern))

  ## Need to handle this case separately, as gregexpr effectively
  ## does not work for this.
  if (length(text) == 0) return(empty_result(text, pattern, ...))

  match <- gregexpr(pattern, text, perl = TRUE, ...)

  num_groups <- length(attr(match[[1]], "capture.names"))

  ## Non-matching strings have a rather strange special form,
  ## so we just treat them differently
  non <- vapply(match, function(m) m[1] == -1, TRUE)
  yes <- !non
  res <- replicate(length(text), list(), simplify = FALSE)
  if (any(non)) {
    res[non] <- list(replicate(num_groups + 1, character(), simplify = FALSE))
  }
  if (any(yes)) {
    res[yes] <- mapply(match1, text[yes], match[yes], SIMPLIFY = FALSE)
  }

  ## Need to assemble the final data frame "manually".
  ## There is apparently no function for this. rbind() is almost
  ## good, but simplifies to a matrix if the dimensions allow it....
  res <- lapply(seq_along(res[[1]]), function(i) {
    lapply(res, "[[", i)
  })

  structure(
    res,
    names = c(attr(match[[1]], "capture.names"), ".match"),
    row.names = seq_along(text),
    class = c("data.frame")
  )
}

match1 <- function(text1, match1) {

  matchstr <- substring(
    text1,
    match1,
    match1 + attr(match1, "match.length") - 1L
  )

  ## substring fails if the index is length zero,
  ## need to handle special case
  if (is.null(attr(match1, "capture.start"))) {
    list(.match = matchstr)

  } else {
    gstart  <- attr(match1, "capture.start")
    glength <- attr(match1, "capture.length")
    gend    <- gstart + glength - 1L

    groupstr <- substring(text1, gstart, gend)
    dim(groupstr) <- dim(gstart)

    c(lapply(seq_len(ncol(groupstr)), function(i) groupstr[, i]),
      list(.match = matchstr)
      )
  }
}

empty_result <- function(text, pattern, ...) {
  match <- regexpr(pattern, text, perl = TRUE, ...)
  num_groups <- length(attr(match, "capture.names"))
  structure(
    replicate(num_groups + 1, list(), simplify = FALSE),
    names = c(attr(match, "capture.names"), ".match"),
    row.names = integer(0),
    class = "data.frame"
  )
}
