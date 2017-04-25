
check_livelog <- function(self, private, which) {
  assert_that(is_count(which) || is_string(which))
  if (is_count(which) && (which < 1 || which > length(private$ids_))) {
    stop("Unknown check selected")
  }
  if (is.character(which) && ! which %in% private$ids_) {
    stop("Unknow check selected")
  }

  make_streamer(private$ids_[[which]], make_status_parser)
  self$update()
  invisible(self)
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

  if (grepl(
    "^(Finished: ABORTED|Finished: ERROR)$",
    response$text[[length(response$text)]]
  )) {
    cat(response$text[[length(response$text)]], "\n", sep = "")
  }
}

#' @importFrom rcmdcheck rcmdcheck

make_status_parser <- function() {

  first <- TRUE
  checking <- FALSE

  ## This is to make sure that `rhub` works with older and newer
  ## rcmdcheck versions as well. Older versions expect a call for each
  ## line. Newer versions just take a block of output.
  formatter <- try(
    ("rcmdcheck" %:::% "check_callback")(top_line = FALSE),
    silent = TRUE
  )
  if (inherits(formatter, "try-error")) {
    cb <- ("rcmdcheck" %:::% "block_callback")(top_line = FALSE)
    formatter <- function(x) cb(paste0(x, "\n"))
  }

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
