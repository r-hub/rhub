# =========================================================================
# API
# =========================================================================

#' Request a new token for submissions to the R Consortium runners
#'
#' To build and check R packages on the RC runners of R-hub, you'll need
#' to verify your email address. R-hub will send a token to your email
#' address, and this token will be stored on your computer.
#'
#' You need to store a token on every computer you want to submit
#' jobs from, either using the same token from the email you got, or
#' you can request additional tokens for the new machines. Your old token
#' will stay valid as well.
#'
#' If you already have a token from a previous version of R-hub, you can
#' reuse that and you don't need to do anything.
#'
#' Run
#' ```
#' rhub:::email_file()
#' ```
#' to see the file rhub uses to store your tokens.
#'
#' @param email Email address to verify We try to detect this, but
#'   if the detection fails, you can specify it explicitly.
#'   If this argument is missing (or `NULL`), then you can specify it
#'   interactively.
#' @param token Token to add. If you already received a token in an email
#'   from R-hub, you can specify that here.
#'
#' @export
#' @family RC runners API

rc_new_token <- function(email = NULL, token = NULL) {
  if (is.null(email) || is.null(token)) {
    if (!is_interactive()) {
      throw(pkg_error("No email or no token and not in interactive mode"))
    }
    return(rc_new_token_interactive(email, token))
  }

  email_add_token(email, token)
  cli::cli_alert_success("Added token for {.val {email}}.", wrap = TRUE)
  cli::cli_alert_info("R-hub tokens are stored at {.path {email_file()}}.")
  invisible()
}

# -------------------------------------------------------------------------
#' Show your tokens for the R Consortium runners
#'
#' Lists all tokens stored on the local machine.
#'
#' @return Data frame with string columns `email` and `token`.
#' @export
#' @family RC runners API

rc_list_local_tokens <- function() {
  list_validated_emails2(message = FALSE, msg_if_empty = FALSE)
}

# -------------------------------------------------------------------------
#' List your repositories created by the R Consortium runners
#'
#' Lists repositories created by [rc_submit()] submissions.
#'
#' @param email Email address. We try to detect this, but
#'   if the detection fails, you can specify it explicitly.
#'
#' @return Data frame with columns:
#'
#'  * `repo_name`: Name of the repository.
#'  * `repo_url`: URL of the repository.
#'  * `builds_url`: URL to the builds of the repository.
#'
#' Additional columns and customized printing will be probably added
#' later to the result.
#'
#' @export
#' @family RC runners API

rc_list_repos <- function(email = NULL) {
  email <- email %||% guess_email(message = TRUE)
  resp <- query("/repos", headers = get_auth_header(email))
  jsonlite::fromJSON(rawToChar(resp$content))
}

# -------------------------------------------------------------------------
#' Submit a package to the R Consortium runners
#'
#' @param path Path to package file or package directory.
#' @param platforms Platforms to checks. See [rhub_platforms()] for a
#'   current list. If not specified, then you can select the platforms
#'   interactively. Must be specified in non-interactive sessions.
#' @param email Email address. You must have a token on the local machhine,
#'   that corresponds to the email address, see [rc_new_token()].
#'   If not specified (or `NULL`) then the email address of the package
#'   maintainer is used.
#' @param confirmation You must set this to `TRUE` to submit a package
#'   from a non-interactive session.
#' @return A list with data about the submission, invisibly.
#' Currently it has:
#'
#'   * `result`: Should be the string `"OK"`.
#'   * `repo_url`: URL to the repository.
#'   * `actions_url`: URL to the builds inside the repository.
#'   * `id`: Build id. This is a string with a randomly generated id.
#'   * `name`: Build name, this is a string, the concatenation of the
#'      build platforms.
#'
#' More fields might be added later.
#'
#' @export
#' @family RC runners API
#' @seealso [rhub_platforms()] for a list of supported platforms.

rc_submit <- function(path = ".", platforms = NULL, email = NULL,
                      confirmation = NULL) {

  if (!isTRUE(confirmation) && !is_interactive()) {
    throw(pkg_error(
      "You need to set {.arg confirmation} to {.val TRUE}
       to submit packages to R-hub from non-interactive R sessions."
    ))
  }

  tryCatch({
    pkg_name <- suppressWarnings(desc::desc_get("Package", file = path)[[1]])
    if (is.na(pkg_name)) stop()
  }, error = function(e) {
    throw(pkg_error(
      "Could not query R package name at {.path {path}}.",
      i = paste(
        "Make sure that {.arg path} is an R package or a directory",
        "containing an R package."
      )
    ))
  })

  email <- email %||% get_maintainer_email(path = path)

  platforms <- select_platforms(platforms)

  if (is_dir(path))  {
    path <- pkgbuild::build(path = path)
  }

  id <- random_id()
  ep <- paste0("/job/", pkg_name)
  form <- list(
    config = curl::form_data(paste(platforms, collapse = ",")),
    id = curl::form_data(id),
    package = curl::form_file(path)
  )

  if (!isTRUE(confirmation)) {
    cat(cli::col_cyan(cli::rule("Confirmation")))
    cli::cli_bullets(c(
      "!" = "Your package will be publicly readable at
        {.url https://github.com/r-hub2}.",
      ">" = "You will need a GitHub account to view the build logs.",
      ">" = "Only continue if you are fine with this.",
      ">" = "See the {.fn rhub_setup} function for an alternative way
        of using R-hub."
    ))
    ans <- trimws(readline(
      prompt = "\nPlease type 'yes' to continue: "
    ))
    cli::cli_text()
    if (ans != 'yes' && ans != "'yes'") {
      throw(pkg_error("Aborted R-hub submission."))
    }
  }

  resp <- query(
    method = "POST",
    ep,
    sse = TRUE,
    data_form = form,
    headers = c(
      get_auth_header(email),
      "content-type" = "multipart/form-data",
      "accept" = "text/event-stream",
      "cache-control" = "no-cache",
      "connection" = "keep-alive"
    )
  )

  resevt <- Filter(function(x) x[["event"]] == "result", resp$sse)
  if (length(resevt) == 0) {
    stop("Invalid response from R-hub server, please report this.")
  }

  retval <- jsonlite::fromJSON(
    resevt[[1]][["data"]],
    simplifyVector = FALSE
  )
  invisible(retval)
}

# =========================================================================
# Internals
# =========================================================================

guess_email <- function(path = ".", message = TRUE) {
  maint <- tryCatch(get_maintainer_email(path), error = function(e) NULL)
  if (!is.null(maint)) {
    if (message) {
      cli::cli_alert_info(
        wrap = TRUE,
        "Using maintainer email address {.val {maint}}."
      )
    }
    return(maint)
  }

  guess <- email_address()
  if (message) {
    cli::cli_alert_info(
      wrap = TRUE,
      "Using email address {.val {guess}}."
    )
  }
  guess
}

get_auth_header <- function(email) {
  valid <- list_validated_emails2(message = FALSE)
  if (! email %in% valid$email) {
    throw(pkg_error(
      "Can't find token for email address {.val {email}}.",
      i = "Call {.code rhub::rc_new_token()} to get a token."
    ))
  }
  token <- valid$token[match(email, valid$email)]
  c("Authorization" = paste("Bearer", token))
}

#' @importFrom cli symbol
#' @importFrom utils menu
#' @importFrom whoami email_address

get_email_to_validate <- function(path) {

  ## Find out email first. List currently validated addresses,
  ## Offer address by whoami::email_address(), and also the
  ## maintainer address, if any.

  valid <- list_validated_emails2(msg_if_empty = FALSE)
  guess <- tryCatch(email_address(), error = function(e) NULL)
  maint <- tryCatch(get_maintainer_email(path), error = function(e) NULL)

  choices <- rbind(
    if (nrow(valid)) cbind(valid = TRUE, valid),
    if (!is.null(guess) && ! guess %in% valid$email) {
      data_frame(valid = FALSE, email = guess, token = NA)
    },
    if (!is.null(maint) && ! maint %in% valid$email && maint != guess) {
      data_frame(valid = FALSE, email = maint, token = NA)
    },
    data_frame(valid = NA, email = "New email address", token = NA)
  )

  ## Only show the menu if there is more than one thing there
  if (nrow(choices) != 1) {
    choices_str <- paste(
      sep = "  ",
      ifelse(
        choices$valid & !is.na(choices$valid),
        cli::col_green(cli::symbol$tick),
        " "
      ),
      choices$email
    )

    cat("\n")
    title <- cli::col_yellow(paste0(
      cli::symbol$line, cli::symbol$line,
      " Choose email address to request token for (or 0 to exit)"
    ))
    ch <- menu(choices_str, title = title)

    if (ch == 0) throw(pkg_error("Cancelled requesting new token"))

  } else {
    ch <- 1
  }

  ## Get another address if that is selected
  if (is.na(choices$valid[ch])) {
    cat("\n")
    email <- readline("Email address: ")
  } else {
    email <- choices$email[ch]
  }

  email
}

list_validated_emails2 <- function(message = is_interactive(),
                                   msg_if_empty = TRUE) {
  file <- email_file()
  res <- if (file.exists(file)) {
    if (message) {
      cli::cli_alert(
        "R-hub tokens are stored at {.path {email_file()}}."
      )
    }
    read_token_file(file)

  } else {
    data.frame(
      email = character(),
      token = character(),
      stringsAsFactors = FALSE
    )
  }
  if (is_interactive() && nrow(res) == 0) {
    if (msg_if_empty) {
      cli::cli_alert_info("No R-hub tokens found.")
    }
    invisible(res)
  } else {
    res
  }
}

#' @importFrom rappdirs user_data_dir

email_file <- function() {
  rhub_data_dir <- user_data_dir("rhub", "rhub")
  file.path(rhub_data_dir, "validated_emails.csv")
}

rc_new_token_interactive <- function(email = NULL, token = NULL, path = ".") {

  if (is.null(email)) email <- get_email_to_validate(path)

  ## Token next. For this we need to make an API query.
  if (is.null(token)) {
    query(
      method = "POST",
      "/user/validate",
      headers = c("content-type" = "application/x-www-form-urlencoded"),
      data = jsonlite::toJSON(list(email = jsonlite::unbox(email)))
    )
    cli::cli_alert_info(
      "Please check your emails for the R-hub access token."
    )
    token <- readline("Token: ")
  }

  ## We got everything now
  rc_new_token(email, token)
}

#' @importFrom utils read.csv write.table

email_add_token <- function(email, token) {
  file <- email_file()

  if (!file.exists(file)) {
    parent <- dirname(file)
    if (!file.exists(parent)) dir.create(parent, recursive = TRUE)
    tokens <- data.frame(
      V1 = character(),
      V2 = character(),
      stringsAsFactors = FALSE
    )

  } else {
    tokens <- read.csv(file, stringsAsFactors = FALSE, header = FALSE)
  }

  if (! email %in% tokens[,1]) {
    tokens <- rbind(tokens, c(email, token))

  } else{
    tokens[match(email, tokens[,1]), 2] <- token
  }

  write_token_file(tokens, file)

  invisible()
}

read_token_file <- function(path) {
  structure(
    read.csv(path, stringsAsFactors = FALSE, header = FALSE),
    names = c("email", "token")
  )
}

write_token_file <- function(tokens, path) {
  write.table(
    tokens,
    file = path,
    sep = ",",
    col.names = FALSE,
    row.names = FALSE
  )
}