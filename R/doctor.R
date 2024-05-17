
#' Check if the current or the specified package is ready to use with R-hub
#'
#' Errors if the package or repository is not set up correctly, and
#' advises on possible solutions.
#'
#' @param gh_url Use `NULL` for the package in the current working
#' directory. Alternatively, use the URL of a GitHub repository that
#' contains an R package that was set up to use with R-hub.
#'
#' @export

rhub_doctor <- function(gh_url = NULL) {
  assert_that(
    is_optional_gh_url(gh_url)
  )

  rpkg_root <- if (is.null(gh_url)) setup_find_r_package()
  git_root <- if (is.null(gh_url)) setup_find_git_root()
  if (is.null(gh_url)) check_rpkg_root(rpkg_root, git_root)

  gh_url <- gh_url %||% doctor_find_gh_url(repo = git_root)
  pat <- doctor_find_pat(gh_url)

  # -----------------------------------------------------------------------
  # Do these up front, concurrently
  # We need the following pieces:
  # 1 check if we are indeed talking to GitHub
  # 2 check that the token is valid, and we have access to the repo
  # 3 check that the token has the right scopes
  # 4 check that the workflow file exists on the default branch
  # 5 check that the workflow exists (e.g. not a fork with disabled actions)
  # 6 check that the workflow is enabled
  # 7 check that the workflow file is the latest version
  #
  # Unfortunately we cannot do all this with a single graphql query, because
  # (AFAICT) you cannot currently query the workflows of a repository with
  # GraphQL.
  #
  # So we'll have
  # - a graphql query for (1), (2), (3), (4), (7)
  # - a REST query for (5) and (6)

  resp <- synchronise(when_all(
    gql = doctor_async_gql(gh_url, token = pat),
    wfl = doctor_async_rest(gh_url, token = pat)
  ))

  doctor_check_github(gh_url, resp$gql)
  doctor_check_pat_scopes(resp$gql)
  doctor_check_workflow(gh_url, resp$gql, resp$wfl)

  cli::cli_alert(
    "WOOT! You are ready to run {.run rhub::rhub_check()} on this package.",
    wrap = TRUE
  )

  invisible(NULL)
}

# TODO: multiple remotes, what if it is not origin?
# TODO: what if there is a remote, but it does not have a URL?

doctor_find_gh_url <- function(repo) {
  remote <- gert::git_info(repo)$remote
  if (is.na(remote)) {
    throw(pkg_error(
      call. = FALSE,
      "Cannot determine GitHub URL from git remote in repository at
       {.file {repo}}. Is your repository on GitHub?",
      i = "If this repository is on GitHub, call
       {.code git remote add origin <github-url>} to add GitHub as a
       remote.",
      i = "Alternatively, specify the GitHub URL of the repository in
       the {.arg gh_url} argument.",
      i = "If it is not on GitHub, then you'll need to put it there.
       Create a new repository at {.url https://github.com/new}."
    ))
  }
  gert::git_remote_info(repo = repo)$url
}

doctor_find_pat <- function(pat_url) {
  pid <- cli_status("Do you have a GitHub personal access token (PAT)?")
  # TODO: get GH URL from git remote, if any
  tryCatch({
    url <- parse_gh_url(pat_url)$pat_url
    pat <- gitcreds::gitcreds_get(url = url)$password },
    gitcreds_nogit_error = function(e) {
      cli::cli_status_clear(pid, result = "failed")
      env <- gitcreds::gitcreds_cache_envvar(url)
      throw(pkg_error(
        call. = FALSE,
        "Could not find a GitHub personal access token (PAT) for {.url {pat_url}}.",
        i = "I also could not find a working git installation. If you
             don't want to install git, but you have a PAT, you can set the
             {.env {env}} environment variable to the PAT.",
        i = "You can read more about PATs at
             {.url https://usethis.r-lib.org/articles/git-credentials.html}."
      )) },
    gitcreds_no_credentials = function(e) {
      cli::cli_status_clear(pid, result = "failed")
      env <- gitcreds::gitcreds_cache_envvar(url)
      throw(pkg_error(
        call. = FALSE,
        "Could not find a GitHub personal access token (PAT) for {.url {pat_url}}.",
        i = "If you have a GitHub PAT, you can use {.run gitcreds::gitcreds_set()}
             to add it to the git credential store, so R-hub can use it.",
        i = "If you don't have a PAT, you can create one by running
             {.run usethis::create_github_token()}.",
        i = "You can read more about PATs at
             {.url https://usethis.r-lib.org/articles/git-credentials.html}."
      )) },
    error = function(e) {
      cli::cli_status_clear(pid, result = "failed")
      throw(e)
    }
  )
  cli::cli_status_clear(pid, result = "clear")
  cli::cli_alert_success("Found GitHub PAT.")

  pat
}

doctor_check_github <- function(gh_url, resp) {
  pid <- cli_status(cli::format_inline("Is the package on GitHub at {.url {gh_url}}?"))
  if (!"x-ratelimit-limit" %in% names(resp$headers)) {
    cli::cli_status_clear(pid, result = "failed")
    throw(pkg_error(
      call. = FALSE,
      "Remote repository at {.url {gh_url}} does not seem like a GitHub
       repository.",
      i = "R-hub only supports GitHub packages in GitHub repositories
       currently.",
      i = "If you think that this is a bug in the {.pkg rhub} package,
       please let us know!"
    ))
  }
  cli::cli_status_clear(pid, result = "clear")
  cli::cli_alert_success(
    "Found repository on GitHub at {.url {gh_url}}.",
    wrap = TRUE
  )
}

# we can assume a GH response at this point

doctor_check_pat_scopes <- function(resp) {
  pid <- cli_status("Does your GitHub PAT have the right scopes?")
  scopes <- trimws(strsplit(
    resp[["headers"]][["x-oauth-scopes"]] %||% "NOPE",
    ",",
    fixed = TRUE
  )[[1]])

  if (identical(scopes, "NOPE")) {
    cli::cli_status_clear(pid, result = "failed")
    throw(pkg_error(
      call. = FALSE,
      "Could not use the PAT to authenticate to GitHub",
      i = "Make sure that the URL and your PAT are correct."
    ))
  }

  if (!"repo" %in% scopes) {
    cli::cli_status_clear(pid, result = "failed")
    throw(pkg_error(
      call. = FALSE,
      "Your PAT does not have a {.code repo} scope.",
      i = "Without a {.code repo} scope R-hub cannot start jobs on GitHub.",
      i = "Change the scopes of the PAT on the GitHub web page, or create
           a new PAT."
    ))
  }
  cli::cli_status_clear(pid, result = "clear")
  cli::cli_alert_success("GitHub PAT has the right scopes.")
}

doctor_check_workflow <- function(gh_url, gql, rest) {
  pid <- cli_status(
    "Does the default branch of your git repo have the R-hub workflow file?"
  )

  if (is.null(gql$workflow)) {
    cli::cli_status_clear(pid, result = "failed")
    throw(pkg_error(
      call. = FALSE,
      "Could not find R-hub's workflow file in the repository at
      {.url {gh_url}}.",
      i = "The workflow file must be at {.path .github/workflows/rhub.yaml}.",
      i = "If you have added and committed the workflow file, you need to
          push the commit to GitHub with {.code git push}.",
      i = if (isTRUE(gql$is_forked))
          "This repository is a fork. Make sure you enabled GitHub Actions
           on it, in the {.emph Actions} tab of the repository web page."
    ))
  }

  if (rest$workflow$state != "active") {
    cli::cli_status_clear(pid, result = "failed")
    throw(pkg_error(
      call. = FALSE,
      "The workflow is disabled.",
      i = "You need to enable it, click on the {.code ...} button at the
           top right corner of the web page of the workflow."
    ))
  }

  cli::cli_status_clear(pid, result = "clear")
  cli::cli_alert_success(
    "Found R-hub workflow in default branch, and it is active."
  )
}

# We need the following pieces:
# - check if we are indeed talking to GitHub
# - check that the token is valid, and we have access to the repo
# - check that the token has the right scopes
# - check that the workflow file exists on the default branch
# - check that the workflow file is the latest version

doctor_async_gql <- function(gh_url, token) {
  url <- parse_gh_url(gh_url)
  query <- glue::glue("{
    repository(owner: \"<url$user>\", name: \"<url$repo>\") {
      workflow_file: object(expression: \"HEAD:.github/workflows/rhub.yaml\") {
        ... on Blob {
          isBinary
          text
        }
      }
      sha: object(expression: \"HEAD\") {
        oid
      }
      branch: defaultBranchRef {
        name
      }
      isFork
    }
  }", .open = "<", .close = ">")
  async_gh_gql_get(url$graphql, query, token)$
    then(function(resp) {
      data <- resp$content$data
      list(
        status_code = resp$status_code,
        headers = resp$headers,
        is_repo = !is.null(data$repository),
        workflow_binary = data$repository$workflow_file$isBinary,
        workflow = data$repository$workflow_file$text,
        sha = data$repository$sha$oid,
        branch = data$repository$branch$name,
        is_fork = data$repository$isFork,
        errors = resp$content$errors
      )
    })
}

# Goal is to
# - check if workflow exist (e.g. not a form with disabled actions)
# - check that workflow is enabled

doctor_async_rest <- function(gh_url, token) {
  url <- parse_gh_url(gh_url)
  ep <- glue::glue("/repos/{url$user}/{url$repo}/actions/workflows/rhub.yaml")
  async_gh_rest_get(url$api, ep, token)$
    then(function(resp) {
      list(
        status_code = resp$status_code,
        headers = resp$headers,
        workflow = resp$content,
        errors = resp$content$errors
      )
    })
}
