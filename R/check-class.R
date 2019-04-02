
#' @title R-hub check ids
#' @section R-hub ids:
#'
#' Every R-hub check has a unique id, that is constructed from the
#' name of the source package archive, and a random string. For example:
#' ```r
#' devtools_2.0.0.tar.gz-fe53bbba85de4a579f6dc3b852bf76a3
#' ```
#'
#' @section R-hub group ids:
#'
#' For every check submission, R-hub also creates a unique check group id.
#' One check group may contain multiple checks. E.g. [check_for_cran()]
#' typically creates three or four check groups. Group ids look the same
#' as individual check ids.
#'
#' @section Abbreviating ids:
#'
#' The rhub package keeps a list of all the checks that it has seen in the
#' current session, and these checks can be also referenced by any unique
#' prefix of the random string part of the id, e.g. in the [get_check()]
#' function. E.g. if rhub already know the devtools check above, then
#' ```r
#' get_check("fe53bbb")
#' ```
#' works.
#'
#' This is only recommended in interactive mode, and we suggest that you
#' always use the full ids when using rhub programmatically.
#' 
#' @name rhub-ids
NULL

#' An `rhub_check` object holds status and results of rhub checks
#'
#' @section Usage:
#' ```
#' ch <- rhub_check$new(ids = NULL, status = NULL, group = NULL)
#' ch$update()
#' ch$print(...)
#' ch$browse(which = NULL)
#' ch$urls(which = NULL)
#' ch$livelog(which = 1)
#' ch$cran_summary()
#' ```
#'
#' @section Arguments:
#' * `ch` An rhub check object. It can be created using [`check()`],
#'   and other check functions including [`check_for_cran`].
#'   See also [last_check()].
#' * `ids` Character vector of check ids.
#' * `status` Check status for `ids` or `group`.
#' * `group` Check group id, string scalar. Either `group` or `ids` must
#'   be non-`NULL`.
#' * `...` Extra arguments are currently ignored.
#' * `which` Which check to show, if the object contains multiple
#'   checks. For `browse` the default is all checks. For `livelog` the
#'   default is the first check. A check can be selected via its number
#'   or id.
#'
#' @section Details:
#'
#' An `rhub_check` object can be created by [check()], [list_my_checks()],
#' or [list_package_checks()]. [last_check()] returns the last check(s)
#' submitted from the current R session. Do not confuse `rhub_check`/`rhub_check_for_cran`
#' (classes) with [check()] or [check_for_cran()] (functions).
#'
#'
#' `ch$update()` updates the status of the check. Printing the check
#' status to the screen does not perform an update, unless the status of
#' the check(s) is unknown.
#'
#' `ch$print()` prints the status of the check(s) to the screen.
#'
#' `ch$cran_summary()` prints text to be copy-pasted in cran-comments.md,
#'  it is especially useful on the output of [`check_for_cran()`].
#'
#' `ch$browse()` opens a tab or window in the default web browser, that points
#' to the detailed logs of the check(s).
#'
#' `ch$urls()` return a [`tibble::tibble`] with URL to the html log, text log and artifacts
#' of the check(s).
#'
#' For both `ch$browse()` and `ch$urls()`, note that the logs and artifacts
#' are not kept forever, they are accessible for a few days after submission.
#'
#' `ch$livelog()` shows the live log of the check. The live log can be
#' interrupted using the usual interruption keyboard shortcut, usually
#' `CTRL+c` or `ESC`.
#'
#' @name rhub_check
NULL

#' @importFrom R6 R6Class

rhub_check <- R6Class(
  "rhub_check",

  public = list(

    initialize = function(ids = NULL, status = NULL, group = NULL)
      check_init(self, private, ids, status, group),

    update = function()
      check_update(self, private),

    print = function(...)
      check_print(self, private, ...),

    web = function(which = NULL)
      check_web(self, private, which),

    browse = function(which = NULL)
      self$web(which),

    urls = function(which = NULL)
      check_urls(self, private, which),

    livelog = function(which = 1)
      check_livelog(self, private, which),

    cran_summary = function()
      check_cran_summary(self, private)
  ),

  private = list(
    ids_ = NULL,                        # character vector of ids
    group_ = NULL,                      # group id
    status_ = NULL,                     # list of status objects, as in DB
    status_updated_ = NULL              # last time status was updated
  )
)

check_init <- function(self, private, ids, status, group) {
  assert_that(
    is_check_ids(ids) || is.null(ids),
    (is_check_ids(group) && length(group) == 1) || is.null(group),
    !is.null(ids) || !is.null(group))

  private$ids_ <- ids
  private$group_ <- group
  private$status_ <- status
  status_updated_ <- Sys.time()
  invisible(self)
}

check_update <- function(self, private) {
  ## If it is a group, we need to get the ids first. This also updates
  ## the status of the individual checks
  if (!is.null(private$group_) && is.null(private$ids_)) {
    grp <- query("GET GROUP STATUS", list(id = private$group_))
    private$ids_ <- map_chr(grp, "[[", "id")
    private$status_ <- grp
    private$status_updated_ <- Sys.time()
    for (i in seq_along(grp)) cache_put(grp[[i]]$id, grp[[i]])
    return(invisible(self))
  }

  ## Check which ones need update. We need to update if we don't know
  ## anything about the id, or if it has not finished yet.
  cached <- lapply(private$ids_, cache_get)
  need_upd <- map_lgl(cached, function(x) {
    is.null(x) || x$status %in% c("created", "in-progress")
  })

  if (any(need_upd)) {
    ## Update
    upd <- query("GET STATUS", data = list(id = private$ids_[need_upd]))
    cached[need_upd] <- upd

    ## Update the cache
    for (i in seq_along(upd)) cache_put(private$ids_[need_upd][i], upd[[i]])
  }

  ## Update the object, we always do this, in case the object is outdated,
  ## but the cache is not
  private$status_ <- cached
  private$status_updated_ <- Sys.time()

  invisible(self)
}

#' @importFrom utils browseURL

check_web <- function(self, private, which) {

  ids <- select_ids(which = which, self = self,
                    private = private)

  urls <- paste0(sub("/api$", "/status/", baseurl()), ids)

  lapply(urls, browseURL)
  invisible(self)
}

check_urls <- function(self, private, which) {

  ids <- select_ids(which = which, self = self,
                    private = private)

  tibble::tibble(html = paste0(sub("/api$", "/status/", baseurl()), ids),
             text = paste0(sub("/api$", "/status/original/", baseurl()),
                           ids),
             artifacts = paste0("https://artifacts.r-hub.io/", ids),
             stringsAsFactors = FALSE)
}

select_ids <- function(which, self, private){
  ids <- if (is.null(which)) {
    private$ids_
  } else if (is.numeric(which)) {
    private$ids_[which]
  } else if (is.character(which)) {
    intersect(private$ids_, which)
  } else {
    stop("Unknown check selected",
         call. = FALSE)
  }

  return(ids)
}

check_cran_summary <- function(self, private) {

  self$update()

  x <- private$status_
  
  statuses <- map_chr(x, "[[", "status")

  if (any(statuses %in% c("in-progress", "created"))) {
    stop(paste("At least one of the builds has not finished yet.",
               "Please wait before calling `cran_summary()` again."), 
         call. = FALSE)
  }
  
  if (any(statuses %in% problem_statuses())) {
    platforms <- lapply(x, "[[", "platform")
    platform_names <- map_chr(platforms, "[[", "name")
    stop(paste("Build failures on platforms:",
               toString(platform_names[statuses %in% problem_statuses()]),
               ". \n",
               "Read the log(s) to fix and if needed ask for help via ",
               "https://docs.r-hub.io/#pkg-dev-help"), 
         call. = FALSE)
  }

  result <- do.call("rbind",
                    lapply(x, rectangle_status))
  systems <- paste0("- R-hub ",
                    vapply(x, function(xx) xx$platform$name, ""),
                    " (",
                    vapply(x, function(xx) xx$platform$rversion, ""),
                    ")")
  lines <- paste0(systems, "\n")

  if ("hash" %in% names(result)){
    message("For a CRAN submission we recommend that you fix all NOTEs, WARNINGs and ERRORs.")
    unique_results <- unique(result[, c("type", "hash")])
    
    makeshift <- structure(
      list(
        package = x$package,
        version = toString(vapply(x, function(xx) xx$platform$name, "")),
        rversion = toString(systems),
        output = list(),
        platform = toString(systems),
        notes = unlist(lapply(unique(unique_results$hash[unique_results$type == "NOTE"]),
                              combine_message, result = result)),
        warnings = unlist(lapply(unique(unique_results$hash[unique_results$type == "WARNING"]),
                                 combine_message, result = result)),
        errors = unlist(lapply(unique(unique_results$hash[unique_results$type == "ERROR"]),
                               combine_message, result = result))
      ),
      class = "rcmdcheck"
    )
    
  } else {
    makeshift <- structure(
      list(
        package = x$package,
        version = toString(vapply(x, function(xx) xx$platform$name, "")),
        rversion = toString(systems),
        output = list(),
        platform = toString(systems),
        notes = NULL,
        warnings = NULL,
        errors = NULL
      ),
      class = "rcmdcheck"
    )
  }
  
  cat("## Test environments\n")
  cat(lines, sep = "")
  cat("\n")
  cat("## R CMD check results\n")
  print(makeshift, header = FALSE)
  
  invisible(self)
}


get_status_part <- function(part, x){
  output <- unlist(x[part])
  if(is.null(output)){
    return("")
  }else{
    output
  }
}

rectangle_status <- function(x){
 
  df <- rbind(data.frame(type = "ERROR",
                         message = get_status_part("errors", x$result),
                         stringsAsFactors = FALSE),
              data.frame(type = "WARNING",
                         message = get_status_part("warnings", x$result),
                         stringsAsFactors = FALSE),
              data.frame(type = "NOTE",
                         message = get_status_part("notes", x$result),
                         stringsAsFactors = FALSE))

  df <- df[df$message != "",]
  
  if(nrow(df) == 0){
    df <- data.frame(package = x$package)
  } else{
    df$hash <- hash_check(df$message)
  }
  
  df$package <- x$package
  df$version <- x$version
  df$submitted <- x$submitted
  df$platform <- paste0(x$platform$name, " (", x$platform$rversion,
                        ")")

  return(df)
}

combine_message <- function(hash, result){
  paste0("On ", toString(result$platform[result$hash == hash]), "\n",
         result$message[result$hash == hash][1])
}

# from rcmdcheck https://github.com/r-lib/rcmdcheck/blob/968fd9ba76ee9b7bf65d192568555ab57160165e/R/parse.R#L110
#' @importFrom digest digest

hash_check <- function(check) {
  cleancheck <- gsub("[^a-zA-Z0-9]", "", first_line(check))
  vapply(cleancheck, digest::digest, "")
}

# from rcmdcheck https://github.com/r-lib/rcmdcheck/blob/afadc6c53310cad2b64e0a58e399efd1ae18d7dd/R/utils.R#L91
first_line <- function(x) {
  l <- strsplit(x, "\n", fixed = TRUE)
  vapply(l, "[[", "", 1)
}

#' Retrieve the result of R-hub checks
#'
#' @param ids One of the following:
#'  - A single R-hub check id.
#'  - A character vector of check ids.
#'  - An R-hub check group id.
#' All ids can be abbreviated, see [R-hub ids][rhub-ids].
#' @return An [rhub_check] object.
#'
#' @section Examples:
#' ```
#' chk <- get_check("915ee61")
#' chk
#' chk$update()
#' chk$browse()
#' chk$cran_summary()
#' chk$urls()
#' ```
#'
#' @export
#' @seealso [list_my_checks()] and [list_package_checks()] to list
#' R-hub checks.

get_check <- function(ids) {
  assert_that(is_check_ids(ids))

  sle <- cache_get_ids(ids)
  grp <- cache_get_group_ids(ids)

  err <- NULL

  ## If we are not sure that it is a group id, then query single ids
  res <- if (length(ids) > 1 || is.na(grp)) {
    ids2 <- ifelse(is.na(sle), ids, sle)
    tryCatch(
      rhub_check$new(ids2)$update(),
      error = function(e) { err <<- e; NULL }
    )
  }

  if (!is.null(res)) return(res)

  ## If there is a chance that it is a group, then we try that as well
  if (length(ids) == 1 && is.na(sle)) {
    ids3 <- if (is.na(grp)) ids else grp
    res <- rhub_check$new(group = ids3)$update()
    res
  } else {
    stop(err)
  }
}
