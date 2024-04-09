check_rpkg_root <- function(rpkg_root, git_root) {
  if (rpkg_root != git_root) {
    throw(pkg_error(
      "R-hub currently requires that your R package is at the root of the
       git repository.",
      i = "Your R package is at {.path {rpkg_root}}.",
      i = "Your git repository root is at {.path {git_root}}."
    ))
  }
}

#' Setup the current R package for use with R-hub
#'
#' It adds or updates the R-hub workflow file to the current package,
#' and advises on next steps.
#'
#' @param overwrite if `TRUE`, [rhub_setup()] will overwrite an already
#' existing workflow file.
#' @return Nothing.
#'
#' @export

rhub_setup <- function(overwrite = FALSE) {
  cli::cli_bullets("Setting up R-hub v2.")
  rpkg_root <- setup_find_r_package()
  git_root <- setup_find_git_root()
  check_rpkg_root(rpkg_root, git_root)

  url <- Sys.getenv(
    "RHUB_WORKFLOW_URL",
    "https://raw.githubusercontent.com/r-hub/actions/v1/workflows/rhub.yaml"
  )
  resp <- synchronise(http_get(url))
  if (resp$status_code != 200) {
    throw(pkg_error(
      "Failed to download R-hub worflow file from GitHub.",
      i = "URL: {.url {url}}.",
      i = "HTTP status: {resp$status_code}.",
      i = "Make sure that you are online and GitHub is up."
    ))
  }
  wf <- resp$content
  wfc <- rawToChar(wf)
  Encoding(wfc) <- "UTF-8"

  updated <- FALSE
  wf_file <- file.path(git_root, ".github", "workflows", "rhub.yaml")
  if (file.exists(wf_file)) {
    wf_current <- read_file(wf_file)
    if (wfc != wf_current) {
      if (overwrite) {
        dir.create(dirname(wf_file), showWarnings = FALSE, recursive = TRUE)
        writeBin(wf, wf_file)
        updated <- TRUE
        cli::cli_bullets(c(
          i = "Updated existing workflow file at {.file {wf_file}},
               as requested"
        ))
      } else {
        throw(pkg_error(
          "Workflow file already exists at {.file {wf_file}}.",
          i = "Use {.code overwrite = TRUE} for overwriting it."
        ))
      }
    } else {
      cli::cli_bullets(c(
        v = "Workflow file {.file {wf_file}} already exists and it is current."
      ))
    }
  } else {
    dir.create(dirname(wf_file), showWarnings = FALSE, recursive = TRUE)
    writeBin(wf, wf_file)
    updated <- TRUE
    cli::cli_bullets(c(
      v = "Created workflow file {.file {wf_file}}."
    ))
  }

  cli::cli_text()
  cli::cli_bullets(c(
    "Notes:",
    "*" = "The workflow file must be added to the {.emph default} branch
          of the GitHub repository.",
    "*" = "GitHub actions must be enabled for the repository. They are
          disabled for forked repositories by default."
  ))
  cli::cli_text()
  cli::cli_bullets(c(
    "Next steps:",
    "*" = "Add the workflow file to git using {.code git add <filename>}.",
    "*" = if (updated) "Commit it to git using {.code git commit}.",
    "*" = if (!updated) "Commit it to git using {.code git commit} (if not committed already).",
    "*" = if (updated) "Push the commit to GitHub using {.code git push}.",
    "*" = if (!updated) "Push the commit to GitHub using {.code git push} (if not pushed already).",
    "*" = "Call {.run rhub::rhub_doctor()} to check that you have set up
           R-hub correctly.",
    "*" = "Call {.run rhub::rhub_check()} to check your package."
  ))

  invisible(NULL)
}

setup_find_r_package <- function() {
  pid <- cli_status("Is the current directory part of an R package?")
  tryCatch(
    rpkg_root <- rprojroot::find_root(rprojroot::is_r_package),
    error = function(e) {
      cli::cli_status_clear(pid, result = "failed")
      throw(pkg_error(
        call. = FALSE,
        "The current directory is not part of an R package.",
        i = "You can create an R package in the current directory if you run
             {.run usethis::create_package('.')}.",
        i = "Alternatively, if you want to use R-hub for a package that is
             already on GitHub, supply the {.arg gh_url} argument to
             {.fun rhub_setup}."
      ))
    }
  )
  cli::cli_status_clear(pid, result = "clear")
  cli::cli_alert_success("Found R package at {.file {rpkg_root}}.")

  rpkg_root
}

setup_find_git_root <- function() {
  pid <- cli_status(
    "Is the current directory part of a git repository?"
  )
  tryCatch(
    git_root <- rprojroot::find_root(rprojroot::is_git_root),
    error = function(e) {
      cli::cli_status_clear(pid, result = "failed")
      throw(pkg_error(
        call. = FALSE,
        "The current R package is not in a git repository.",
        i = "You can create a git repository for the current package or
             project if you run {.run usethis::use_git()}.",
        i = "Alternatively, if you want to use R-hub for a package that is
             already on GitHub, supply the {.arg gh_url} argument to
             {.fun rhub_setup}."
      ))
    }
  )
  cli::cli_status_clear(result = "clear")
  cli::cli_alert_success("Found git repository at {.file {git_root}}.")

  git_root
}
