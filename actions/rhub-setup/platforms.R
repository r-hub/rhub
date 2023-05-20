
get_platforms <- function() {
  source("json.R")
  platforms <- json$parse_file("platforms.json")
  names(platforms) <- vcapply(platforms, "[[", "name")
  platforms
}

parse_args <- function(args) {
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
    json$parse(args),
    error = function(e) {
      list(platforms = trimws(strsplit(args, ",")[[1]]))
    }
  )

  list(config = config, containers = containers, platforms = platforms)
}

match_platforms <- function(config) {
  platforms <- get_platforms()
  cnt <- plt <- character()
  for (p in config$platforms) {
    done <- FALSE
    for (np in platforms) {
      if (p == np$name || p %in% np$`cran-names` || p %in% np$aliases) {
        done <- TRUE
        if (np$type == "container") {
          cnt <- c(cnt, np$name)
        } else {
          plt <- c(plt, np$name)
        }
      }
    }
    if (!done) {
      stop("Unknown R-hub platform: ", p)
    }
  }

  cnt <- platforms[unique(cnt)]
  plt <- platforms[unique(plt)]

  cnt <- lapply(cnt, "[", c("name", "container"))
  plt <- lapply(plt, "[", c("name", "os", "r-version"))

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
    x <- sub("\\", "\\\\", x, fixed = TRUE)
    x <- sub("\"", "\\\"", x, fixed = TRUE)
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
          if (length(x) != 1) "[",
          paste0(vcapply(x, to_json), collapse = ","),
          if (length(x) != 1) "]"
        )
      }
    },
    stop("Cannot convert a ", tx, " object to JSON.")
  )
}

main <- function() {
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
