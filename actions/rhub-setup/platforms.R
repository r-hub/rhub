
strict_mode <- function() {
  options(
    useFancyQuotes = FALSE,
    warn = 2,
    warnPartialMatchArgs = TRUE,
    warnPartialMatchAttr = TRUE,
    warnPartialMatchDollar = TRUE
  )
}

get_platforms <- function() {
  source("json.R")
  json[["parse_file"]]("platforms.json")
}

parse_args <- function(args) {
  source("json.R")
  containers <- "--containers" %in% args
  platforms <- "--not-containers" %in% args
  if (containers + platforms != 1) {
    stop("Needs exactly one of `--containers` and `--not-containers`")
  }
  args <- setdiff(args, c("--containers", "--not-containers"))
  if (length(args) == 0) {
    stop("No configuration argument")
  }
  if (length(args) > 1) {
    stop("Needs exactly one configuration argument")
  }

  config <- tryCatch(
    json[["parse"]](args),
    error = function(e) {
      list(platforms = trimws(strsplit(args, ",")[[1]]))
    }
  )

  list(config = config, containers = containers, platforms = platforms)
}

match_r_version <- function(p) {
  if (p[["r-version"]] != "default") {
    p[["r-version"]]
  } else if (startsWith(p$name, "r-devel-")) {
    "devel"
  } else if (startsWith(p$name, "r-patched-")) {
    "next"
  } else if (startsWith(p$name, "r-release-")) {
    "release"
  } else if (startsWith(p$name, "r-oldrel-")) {
    "oldrel"
  } else {
    "devel"
  }
}

match_platforms <- function(config) {
  platforms <- get_platforms()
  cnt <- plt <- list()
  for (p in config$platforms) {

    # Allow a simple character form specifying a platform or alias
    if (is.character(p)) {
      p <- list(name = p, "r-version" = "default")
    }
    done <- FALSE

    # Collect **all** matching platforms
    for (np in platforms) {
      if (p$name %in% c(np[["name"]], np[["cran-names"]], np[["aliases"]])) {
        done <- TRUE
        np[["label"]] <- if (np[["name"]] != p[["name"]]) {
          paste0(np[["name"]], " (", p[["name"]], ")")
        } else {
          np[["name"]]
        }
        if (!is.null(np[["r-version"]]) && np[["r-version"]] == "*") {
          np[["r-version"]] <- match_r_version(p)
          np[["label"]] <- paste0(np[["label"]], " (R-", np[["r-version"]], ")")
        }
        if (np[["type"]] == "container") {
          cnt <- c(cnt, list(np))
        } else {
          plt <- c(plt, list(np))
        }
      }
    }
    if (!done) {
      stop("Unknown R-hub platform: ", p$name)
    }
  }

  cnt <- unique(cnt)
  plt <- unique(plt)
  cnt <- lapply(cnt, "[", c("label", "name", "container"))
  plt <- lapply(plt, "[", c("label", "name", "os", "r-version"))

  for (i in seq_along(cnt)) {
    cnt[[i]]$`job-config` <- to_json(cnt[[i]])
  }
  for (i in seq_along(plt)) {
    plt[[i]]$`job-config` <- to_json(plt[[i]])
  }

  list(containers = unname(cnt), platforms = unname(plt))
}

vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, FUN.VALUE = character(1), ...)
}

to_json <- function(x) {

  no_na <- function(x) {
    if (anyNA(x)) {
      stop("`NA` values are not allowed when converting config to JSON.")
    }
    x
  }

  esc <- function(x) {
    x <- gsub("\\", "\\\\", x, fixed = TRUE)
    x <- gsub("\"", "\\\"", x, fixed = TRUE)
    x
  }

  is_named <- function(x) {
    if (is.null(names(x))) {
      return(FALSE)
    }
    if (any(names(x) == "")) {
      stop("All elements of lists must be named when converting to JSON")
    }
    if (any(duplicated(names(x)))) {
      stop("Duplicate names in lists are not allowed when converting to JSON")
    }
    TRUE
  }

  tx <- typeof(x)
  switch(
    tx,
    "NULL" = "null",
    "logical" = ,
    "integer" = ,
    "double" = {
      paste0(
        if (length(x) != 1) "[",
        tolower(paste(no_na(x), collapse = ",")),
        if (length(x) != 1) "]"
      )
    },
    "character" = {
      paste0(
        if (length(x) != 1) "[",
        paste0('"', esc(no_na(x)), '"', collapse = ","),
        if (length(x) != 1) "]"
      )
    },
    "list" = {
      if (is_named(x)) {
        paste0(
          "{",
          paste0('"', names(x), "\":", vcapply(x, to_json), collapse = ","),
          "}"
        )
      }else {
        paste0(
          "[",
          paste0(vcapply(x, to_json), collapse = ","),
          "]"
        )
      }
    },
    stop("Cannot convert a ", tx, " object to JSON.")
  )
}

main <- function() {
  strict_mode()
  args <- parse_args(commandArgs(TRUE))
  run <- match_platforms(args$config)
  if (args$containers) {
    writeLines(to_json(run$containers))
  } else if (args$platforms) {
    writeLines(to_json(run$platforms))
  }
}

if (is.null(sys.calls())) {
  main()
}

# -------------------------------------------------------------------------

if (Sys.getenv("TESTTHAT") == "") {
  test_that <- function(...) invisible()
}

test_that("to_json", {
  expect_snapshot({
    to_json(1)
    to_json(1:4)
    to_json(FALSE)
    })
})
