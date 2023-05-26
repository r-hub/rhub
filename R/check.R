
#' Check a package on R-hub
#'
#' @param gh_url GitHub URL of a package to check, or `NULL` to check
#'   the package in the current directory.
#' @param platforms Platforms to use, a character vector. Use `NULL` to
#'   select from a list in interactive sessions. See [rhub_platforms()].
#' @param r_versions Which R version(s) to use for the platforms that
#'   supports multiple R versions. This arguemnt is not implemented yet.
#' @param branch Branch to use to run R-hub. Defaults to the current
#'   branch if `gh_url` is `NULL`. Otherwise defaults to `"main"`. Note that
#'   this branch also need to include the `rhub.yaml` workflow file.
#' @return TODO
#'
#' @export

rhub_check <- function(gh_url = NULL, platforms = NULL, r_versions = NULL,
                       branch = NULL) {
  assert_that(
    is_optional_gh_url(gh_url),
    is.null(platforms) || is_character(platforms),
    is_optional_string(branch)
  )

  git_root <- if (is.null(gh_url)) setup_find_git_root()
  pat_url <- gh_url %||% "https://github.com"
  pat <- doctor_find_pat(pat_url)
  gh_url <- gh_url %||% doctor_find_gh_url(repo = git_root)

  if (is.null(branch)) {
    if (!is.null(git_root)) {
      branch <- gert::git_branch(repo = git_root)
    } else {
      branch <- "main"
    }
  }

  tryCatch(
    plat <- rhub_platforms(),
    error = function(e) {
      throw(parent = e, pkg_error(
        "Failed to download the list of R-hub platforms.",
        i = "Make sure that you are online and Github is also online."
      ))
    }
  )

  if (is.null(platforms)) {
    if (!interactive()) {
      throw(pkg_error(
        "{.arg platforms} argument is missing for {.fun rhub_check}.",
        i = "You need to specify {.arg platforms} in non-interactive
             sessions"
      ))
    }
    cli::cli_text()
    cli::cli_text(
      "Available platforms
       (see {.code rhub2::rhub_platforms()} for details):"
    )
    cli::cli_text()
    cli::cli_verbatim(paste(
      cli::ansi_strtrim(format(summary(plat))),
      collapse = "\n"
    ))
    pnums <- trimws(readline(
      prompt = "\nSelection (comma separated numbers, 0 to cancel): "
    ))
    if (pnums == "" || pnums == "0") {
      throw(pkg_error("R-hub check cancelled"))
    }
    pnums <- unique(trimws(strsplit(pnums, ",", fixed = TRUE)[[1]]))
    pnums2 <- suppressWarnings(as.integer(pnums))
    bad <- is.na(pnums2) | pnums2 < 1 | pnums2 > nrow(plat)
    if (any(bad)) {
      throw(pkg_error(
        "Invalid platform number{?s}: {.val {pnums[bad]}}."
      ))
    }
    platforms <- plat$name[pnums2]

  } else {
    platforms <- unique(platforms)
    bad <- !platforms %in% unlist(plat$name, plat$aliaeses)
    if (any(bad)) {
      throw(pkg_error(
        "Unknown platform{?s}: {.val {platforms[bad]}}.",
        i = "See {.run rhub::rhub_platforms()} for the list of platforms"
      ))
    }
  }

  url <- parse_gh_url(gh_url)
  ep <- glue::glue("/repos/{url$user}/{url$repo}/actions/workflows/rhub.yaml/dispatches")
  config <- list(platforms = platforms)
  name <- paste(platforms, collapse = ", ")
  id <- random_id()
  data <- list(
    ref = branch,
    inputs = list(
      config = jsonlite::toJSON(config, auto_unbox = TRUE),
      name = name,
      id = id
    )
  )
  jsondata <- jsonlite::toJSON(data, auto_unbox = TRUE)

  resp <- gh_rest_post(url$api, ep, token = pat, data = jsondata)

  if (resp$status_code != 204) {
    throw(pkg_error(
      ":( Failed to start check: {resp$content$message}.",
      i = "If you think this is a bug in the {.pkg rhub2} package, please
           open an issues at {.url https://github.com/r-hub/rhub/issues}."
    ))
  }

  aurl <- paste0("https://", url$host, "/", url$user, "/", url$repo, "/actions")
  cli::cli_text()
  cli::cli_bullets(c(
    "v" = "Check started: {name} ({id}).",
    " " = "See {.url {aurl}} for live output!"
  ))


  invisible(NULL)
}
