
is_character <- function(x) {
  if (!is.character(x)) {
    structure(
      FALSE,
      msg = "{.arg {(.arg)}} must be a character vector without {.code NA},
             but it is {.type {x}}",
      env = environment()
    )
  } else if (anyNA(x)) {
    structure(
      FALSE,
      msg = "{.arg {(.arg)}} must be a character vector without {.code NA},
             but it has {sum(is.na(x))} {.code NA} value{?s}.",
      env = environment()
    )
  } else {
    TRUE
  }
}

is_string <- function(x) {
  if (is.character(x) && length(x) == 1 && !is.na(x)) return(TRUE)
  if (is.character(x) && length(x) == 1 && is.na(x)) {
    structure(
      FALSE,
      msg = "{.arg {(.arg)}} must not be {.code NA}.",
      env = environment()
    )
  } else {
    structure(
      FALSE,
      msg = "{.arg {(.arg)}} must be a string (character scalar),
           but it is {.type {x}}.",
      env = environment()
    )
  }
}

is_optional_string <- function(x) {
  if (is.null(x) || is_string(x)) return(TRUE)
  structure(
    FALSE,
    msg = "{.arg {(.arg)}} must be a path (character scalar),
           but it is {.type {x}}.",
    env = environment()
  )
}

is_optional_gh_url <- function(x) {
  if (is.null(x)) return(TRUE)

  if (!is_string(x)) {
    structure(
      FALSE,
      msg = "{.arg gh_url} must be a character string.
             You supplied {.type {x}}."
    )
  } else if (!grepl("^https?://", x)) {
    structure(
      FALSE,
      msg = "{.arg gh_url} must be an HTTP or HTTPS URL.
             You supplied: {.val {x}}."
    )
  } else {
    TRUE
  }
}
