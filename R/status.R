
#' Query the status of an r-hub check
#'
#' @param id The check id, an r-hub status URL, or the object retured
#'   by [check()] or one of its variants.
#' @return A list with the status of the check. It has entries:
#'   `status`, `submitted` and `duration`. Currently the duration is
#'   only filled when the build has finished. If a list of checks are
#'   queried, then a list of statuses are returned.
#'
#' @export

status <- function(id = NULL) {

  id <- id %||% package_data$last_handle
  if (is.null(id)) stop("Cannot find last rhub check")

  res <- query("GET STATUS", data = list(id = handle_id(id)))

  for (i in seq_along(res)) class(res[[i]]) <- "rhub_status"

  class(res) <- "rhub_status_list"

  res
}

check_status <- function(id, interactive = interactive()) {
  if (interactive) {
    make_streamer(handle_id(id), make_status_parser)
    invisible(status(id))
  } else {
    cat("\nUse `status()` to check the status of these checks.\n")
    invisible(id)
  }
}

make_streamer <- function(id, parser_factory) {

  if (length(id) > 1) {
    warning("Only first submission is streamed")
    id <- id[1]
  }

  start <- 0
  parser <- parser_factory()

  spinner <- c("-", "/", "|", "\\")
  spin <- function() {
    cat("\r", spinner[1], sep = "")
    spinner <<- c(spinner[-1], spinner[1])
  }

  errors <- 100

  repeat {
    response <- tryCatch(
      query(
        "LIVE LOG",
        params = list(id = id),
        query = list(start = start)
      ),
      error = function(e) {
        if (errors > 0) {
          errors <- errors - 1
          list(text = list(), more = TRUE, size = start)
        } else {
          stop("Internal R-hub error")
          list(text = list(), more = FALSE)
        }
      }
    )

    for (i in response$text) parser(i)
    if (!response$more) break;
    start <- response$size
    for (i in 1:5) { Sys.sleep(0.1); spin() }
  }

  cat("\r    \n")
}

#' @importFrom rcmdcheck rcmdcheck

make_status_parser <- function() {

  first <- TRUE
  checking <- FALSE
  formatter <- ("rcmdcheck" %:::% "check_callback")(top_line = FALSE)

  function(x) {

    ## Make sure we are at the beginning of the line
    cat("\r")

    if (first) {
      header_line("Build started")
      first <<- FALSE
    }

    ## Get rid of potential \r characters
    x <- gsub("[\r]+", "", x)

    ## Checking (already, and still)

    if (checking) {
      if (grepl("^Status: ", x)) {
        checking <<- FALSE
        return(formatter(x))
      } else {
        return(formatter(x))
      }
    }

    ## Not checking (yet, or any more)

    if (grepl("^>>>>>=====+ Running R CMD check", x)) {
      checking <<- TRUE
      x <- sub("^>>>>>=+ ", "", x)
      header_line(x)

    } else if (grepl("^>>>>>=====", x)) {
      x <- sub("^>>>>>=+ ", "", x)
      header_line(x)

    } else if (grepl("^\\+R-HUB-R-HUB-R-HUB", x)) {
      x <- sub("^\\+R-HUB-R-HUB-R-HUB", "", x)

    } else {
      ## print nothing
    }
  }
}
