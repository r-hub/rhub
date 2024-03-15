# =========================================================================
#' @export

rc_new_token <- function(email = NULL, token = NULL) {
  if (is.null(email) || is.null(token)) {
    if (!is_interactive()) {
      throw(pkg_error("No email or no token and not in interactive mode"))
    }
    return(rc_new_token_interactive(email, token))
  }

  email_add_token(email, token)
  cli::cli_alert_success("Added token for {.val email}.", wrap = TRUE)
  cli::cli_alert_info("R-hub tokens are stored at {.path {email_file()}}.")
  invisible()
}

# -------------------------------------------------------------------------
#' @export

rc_list_repos <- function(email = NULL) {
  email <- email %||% guess_email(message = TRUE)
  resp <- query("/repos", headers = get_auth_header(email))
  jsonlite::fromJSON(rawToChar(resp$content))
}

# -------------------------------------------------------------------------
#' @export

rc_submit <- function(path = ".", platforms = NULL, email = NULL,
                      r_versions = NULL) {
  pkg_name <- desc::desc_get("Package", file = path)[[1]]
  if (is.na(pkg_name)) {
    throw(pkg_error(
      "Could not query R package name at {.path {path}}.",
      i = paste(
        "Make sure that {.arg path} is an R package or a directory",
        "contaiing an R package."
      )
    ))
  }

  email <- email %||% get_maintainer_email(path = path)

  if (is_dir(path))  {
    path <- pkgbuild::build(path = path)
  }

  query("job/")
}

# =========================================================================
# Internals
# =========================================================================

guess_email <- function(path = ".", message = TRUE) {
  valid <- list_validated_emails2(message = FALSE)
  maint <- tryCatch(get_maintainer_email(path), error = function(e) NULL)
  if (!is.null(maint)) {
    if (message) {
      cli::cli_alert_info(
        wrap = TRUE,
        "Using maintainer email address {.val {maint}}."
      )
      return(maint)
    }
  }

  guess <- email_address()
  if (message) {
    cli::cli_alert_info(
      wrap = TRUE,
      "Using email address {.val {guess}}."
    )
  }
}

get_auth_header <- function(email) {
  valid <- list_validated_emails2(message = FALSE)
  if (! email %in% valid$email) {
    throw(pkg_error(
      "Can't find token for email address {.val {guess}}.",
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
  guess <- email_address()
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

    structure(
      read.csv(file, stringsAsFactors = FALSE, header = FALSE),
      names = c("email", "token")
    )
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

rc_new_token_interactive <- function(email, token, path = ".") {

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

  write.table(
    tokens,
    file = file,
    sep = ",",
    col.names = FALSE,
    row.names = FALSE
  )

  invisible()
}
