
#' List all R-hub platforms
#'
#' The platforms are pretty-printed in a short format. Use
#' `as.data.frame(platforms())` to get all available platform metadata.
#'
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom crayon green cyan
#' @examples
#' \dontrun{
#' platforms()
#' as.data.frame(platforms())
#' }

platforms <- function() {
  json <- query("GET PLATFORMS", as = "text")
  pls <- fromJSON(json, simplifyDataFrame = TRUE)
  pls <- pls[order(pls$name), , drop = FALSE]
  class(pls) <- c("rhub_platforms", class(pls))
  pls
}

#' @export

print.rhub_platforms <- function(x, ...) {
  res <- paste(cyan(x$name), green(x$description), sep = ":\n  ")
  cat(res, sep = "\n")
  invisible(x)
}

match_platform <- function(platform) {
  all_platforms <- platforms()
  if (is.null(platform)) {
    if (is_interactive()) {
      select_platform_interactively(all_platforms)
    } else {
      all_platforms$name[1]
    }

  } else {
    if (! all(platform %in% all_platforms$name)) {
      stop("Unknown R-hub platform, see rhub::platforms() for a list")
    }
    platform
  }
}

select_platform_interactively <- function(platforms) {

  choices <- paste0(
    platforms$description,
    crayon::green(" (", platforms$name, ")", sep = "")
  )

  cat("\n")
  title <- crayon::yellow(paste0(
    symbol$line, symbol$line,
    " Choose build platform"
  ))
  ch <- menu(choices, title = title)
  cat("\n")
  if (ch == 0) stop("R-hub check aborted", call. = FALSE)

  platforms$name[ch]
}
