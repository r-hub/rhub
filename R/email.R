
#' Validate an email address on r-hub
#'
#' To build and check R packages on r-hub, you need to validate your
#' email address. This is because r-hub sends out emails about build
#' results.
#'
#' The `rhub` package stores validated email addresses in a user
#' configuration file, at a platform-dependent location.
#' On your current platform the file is at
#' \Sexpr{rhub:::email_file()}.
#'
#' To validate a new email address, call this function from an interactive
#' R session, without any arguments.
#'
#' To add an email address that was validated before (probably on another
#' machine), to the configuration file, call this function with the `email`
#' and `token` arguments.
#'
#' @param email The email address to validate.
#' @param token Token obtained from `rhub`, to validate the email address.
#'
#' @family email validation
#' @export
#' @importFrom jsonlite unbox

validate_email <- function(email = NULL, token = NULL) {

  if (is.null(email) || is.null(token)) {
    if (!is_interactive()) {
      stop("No email or no token and not in interactive mode")
    }
    return(validate_email_interactive(email, token))
  }

  assert_that(is_email(email))
  assert_that(is_token(token))

  email_add_token(email, token)
  message("Token added for ", sQuote(email))
  cat("\n")

  invisible()
}

#' @importFrom clisymbols symbol
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
        crayon::green(symbol$tick),
        " "
      ),
      choices$email
    )

    cat("\n")
    title <- crayon::yellow(paste0(
      symbol$line, symbol$line,
      " Choose email address to (re)validate (or 0 to exit)"
    ))
    ch <- menu(choices_str, title = title)

    if (ch == 0) stop("Cancelled email validation", call. = FALSE)

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

validate_email_interactive <- function(email, token, path = ".") {

  if (is.null(email)) email <- get_email_to_validate(path)
  assert_that(is_email(email))

  ## Token next. For this we need to make an API query.
  if (is.null(token)) {
    query("VALIDATE EMAIL", data = list(email = unbox(email)))
    message(crayon::yellow(
      "Please check your emails for the r-hub access token."
    ))
    token <- readline("Token: ")
  }
  assert_that(is_token(token))

  ## We got everything now
  validate_email(email, token)
}

#' List validated email addresses
#'
#' @return A data frame with two columns: `email` and `token`.
#'   If in interactive mode, and there are no validated email addresses,
#'   then a message is printed and the data frame is returned invisibly.
#'
#' @family email validation
#' @export

list_validated_emails <- function() {
  list_validated_emails2()
}

list_validated_emails2 <- function(msg_if_empty = TRUE) {
  file <- email_file()
  res <- if (file.exists(file)) {
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
    if (msg_if_empty) message("No validated emails found.")
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

#' @importFrom utils read.csv

email_get_token <- function(email) {
  file <- email_file()
  if (! file.exists(file)) return(NULL)

  tokens <- read.csv(file, stringsAsFactors = FALSE, header = FALSE)
  if (! email %in% tokens[,1]) return(NULL)

  tokens[match(email, tokens[,1]), 2]
}

## If it exists already, then overwrites

#' @importFrom utils read.csv write.table

email_add_token <- function(email, token) {

  assert_that(is_email(email))
  assert_that(is_token(token))

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
