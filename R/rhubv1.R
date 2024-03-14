
baseurl <- function() {
  paste0(Sys.getenv("RHUB_SERVER", "https://builder.r-hub.io"), "/api")
}

rhub_server <- baseurl

endpoints <- list(
  c("GET PLATFORMS",       "GET",  "/platform/list",        FALSE),
  c("VALIDATE EMAIL",      "POST", "/check/validate_email", FALSE),
  c("SUBMIT PACKAGE",      "POST", "/check/submit",         FALSE),
  c("GET STATUS",          "POST", "/status",               FALSE),
  c("GET GROUP STATUS",    "GET",  "/status/group/:id",     FALSE),
  c("LIST BUILDS EMAIL",   "GET",  "/list/:email",          TRUE),
  c("LIST BUILDS PACKAGE", "GET",  "/list/:email/:package", TRUE),
  c("LIVE LOG",            "GET",  "/livelog/text/:id",     FALSE)
)

default_headers <- c(
  "Accept"       = "application/json",
  "Content-Type" = "application/json",
  "User-Agent"   = "R-hub client"
)

#' @importFrom httr GET POST DELETE add_headers
#' @importFrom jsonlite toJSON

query <- function(endpoint, params = list(), data = NULL,
                  query = list(), headers = character(), as = NULL) {

  ep <- get_endpoint(endpoint, params)
  headers <- update(
    update(default_headers, ep$headers),
    as.character(headers))

  url <- paste0(baseurl(), ep$path)

  json <- if (!is.null(data)) toJSON(data)

  response <- if (ep$method == "GET") {
    GET(url, add_headers(.headers = headers), query = query)

  } else if (ep$method == "POST") {
    POST(url, add_headers(.headers = headers), body = json, query = query)

  } else if (ep$method == "DELETE") {
    DELETE(url, add_headers(.headers = headers), query = query)

  } else {
    stop("Unexpected HTTP verb, internal rhub error")
  }

  report_error(response)

  parse_response(response, as = as)
}

get_endpoint <- function(endpoint, params) {

  idx <- match(endpoint, vapply(endpoints, "[[", "", 1))
  if (is.na(idx)) stop("Unknown API endpoint: ", sQuote(endpoint))

  method <- endpoints[[idx]][2]
  path <- endpoints[[idx]][3]

  colons <- re_match_all(path, ":[a-zA-Z0-9_]+")$.match[[1]]

  for (col in colons) {
    col1 <- substring(col, 2)
    value <- params[[col1]] %||% stop("Unknown API parameter: ", col)
    path <- gsub(col, value, path, fixed = TRUE)
  }

  headers <- if (endpoints[[idx]][[4]]) {
    if (is.null(params$token)) {
      stop("Cannot find token, email address is not validated?")
    }
    c("Authorization" = paste("token", params$token))
  }

  list(method = method, path = path, headers = headers)
}

#' @importFrom httr headers content
#' @importFrom jsonlite fromJSON

parse_response <- function(response, as = NULL) {

  content_type <- headers(response)$`content-type`

  if (is.null(content_type) || length(content_type) == 0) {
    content(response, as = "text")

  } else if (grepl("^application/json", content_type, ignore.case = TRUE)) {
    if (is.null(as)) {
      fromJSON(content(response, as = "text"), simplifyVector = FALSE)
    } else {
      content(response, as = as)
    }

  } else {
    content(response, as = "text")
  }
}

#' @importFrom assertthat assert_that on_failure<-

is_pkg_dir <- function(path) {
  file.exists(path) &&
    file.info(path)$isdir &&
    file.exists(file.path(path, "DESCRIPTION"))
}

is_pkg_tarball <- function(path) {
  file.exists(path) &&
    grepl("\\.tar\\.gz", path)
}

is_pkg_dir_or_tarball <- function(path) {
  is_pkg_tarball(path) || is_pkg_dir(path)
}

on_failure(is_pkg_dir_or_tarball) <- function(call, env) {
  paste0(
    deparse(call$path),
    " is not an R package directory or source R package"
  )
}

is_string <- function(x) {
  !is.null(x) &&
    is.character(x) &&
    length(x) == 1 &&
    !is.na(x)
}

on_failure(is_string) <- function(call, env) {
  paste0(deparse(call$x), " is not a string")
}

is_string_or_null <- function(x) {
  is_string(x) || is.null(x)
}

on_failure(is_string_or_null) <- function(call, env) {
  paste0(deparse(call$x), " is not a string and not NULL")
}

is_email <- function(x) {
  assert_that(is_string(x))
  grepl(".@.", x)
}

on_failure(is_email) <- function(call, env) {
  paste0(deparse(call$x), " is not an email address")
}

is_flag <- function(x) {
  !is.null(x) &&
    is.logical(x) &&
    length(x) == 1 &&
    !is.na(x)
}

on_failure(is_flag) <- function(call, env) {
  paste0(deparse(call$x), " is not a flag (length one logical)")
}

is_named <- function(x) {
  length(names(x)) == length(x) &&
    all(names(x) != "")
}

on_failure(is_named) <- function(call, env) {
  paste0(deparse(call$x), " does not have names")
}

is_token <- function(x) {
  assert_that(is_string(x))
  grepl("[a-zA-Z0-9]{6}", x, perl = TRUE)
}

on_failure(is_token) <- function(call, env) {
  paste0(deparse(call$x), " does not look like an R-hub token")
}

is_check_ids <- function(x) {
  is.character(x) && length(x) >= 1 && all(x != "")
}

on_failure(is_check_ids) <- function(call, env) {
  paste0(deparse(call$x), " is not a vector of check ids")
}

is_count <- function(x) {
  is.numeric(x) && length(x) == 1 && as.integer(x) == x
}

on_failure(is_count) <- function(call, env) {
  paste0(deparse(call$x), " is not a count (length 1 integer)")
}

as_timeout <- function(x) {
  if (inherits(x, "difftime")) return(x)
  as.difftime(as.double(x), units = "secs")
}

is_timeout <- function(x) {
  inherits(x, "difftime")  && length(x) == 1  && !is.na(x)
}

on_failure(is_timeout) <- function(call, env) {
  paste0(deparse(call$x), " must be a timeout, a 'difftime' constant")
}

#' @importFrom withr with_dir
#' @importFrom callr rcmd_safe

build_package <- function(path, tmpdir) {

  path <- normalizePath(path)

  dir.create(tmpdir)
  file.copy(path, tmpdir, recursive = TRUE)

  ## If not a tar.gz, build it. Otherwise just leave it as it is.
  if (file.info(path)$isdir) {
    build_status <- with_dir(
      tmpdir,
      rcmd_safe("build", basename(path))
    )
    unlink(file.path(tmpdir, basename(path)), recursive = TRUE)
    report_system_error("Build failed", build_status)
  }

  file.path(
    tmpdir,
    list.files(tmpdir, pattern = "\\.tar\\.gz$")
  )
}

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
#' ch$get_ids()
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
#' `ch$get_ids()` returns the check ids. These can be used to query if a
#' check has finished.
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

    get_ids = function() private$ids_,

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

  
  result <- result[!is.na(result$type),]
  
  if (nrow(result) > 0){
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
    df <- data.frame(package = x$package,
                     type = NA,
                     message = NA, 
                     hash = NA)
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

  short <- grep(re_id, ids, invert = TRUE, value = TRUE)
  if (length(short) &&
      length(package_data$ids) == 0 &&
      length(package_data$groups) == 0) {
    stop(
      "Short check id '", short[1], "' ",
      if (length(short) > 1) paste0("(and ", length(short)-1, " more) "),
      "can only be used for cached ids, and no ids are cached yet.\n",
      "  Try calling `list_my_checks()` or `list_package_checks()` first."
    )
  }
  
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

re_id <- "-[0-9a-f]{32}$"

#' Check an R-package on R-hub, for a CRAN submission
#'
#' This function calls [check()] with arguments and platforms, that
#' are suggested for a CRAN submission.
#'
#' In particular, if `platforms` is `NULL` (the default), then
#' * It checks the package on Windows, and Linux.
#' * It checks the package on R-release and R-devel.
#' * It uses the `--as-cran` argument to `R CMD check`.
#' * It requires all dependencies, including suggested ones.
#' 
#' @details This function is wrapped by `devtools::check_rhub()` which you 
#' might find useful if you load `devtools` via your .Rprofile (see `usethis::use_devtools()`).
#'
#' @param check_args Arguments for `R CMD check`. By default `--as-cran`
#'   is used.
#' @param env_vars Character vector of environment variables to set on the builder. 
#'   By default `_R_CHECK_FORCE_SUGGESTS_="true"` is set, to require all packages used.
#'   `_R_CHECK_CRAN_INCOMING_USE_ASPELL_="true"` is also set, to use the
#'   spell checker.
#' @param ... Additional arguments are passed to [check()].
#' @inheritParams check
#' @return An [rhub_check] object.
#'
#' @export
#' @examples
#' \dontrun{
#' ch <- check_for_cran("package", show_status = FALSE)
#' ch$update()
#' ch$livelog(3)
#' }

check_for_cran <- function(
  path = ".", email = NULL, check_args = "--as-cran",
  env_vars = c("_R_CHECK_FORCE_SUGGESTS_" = "true",
    "_R_CHECK_CRAN_INCOMING_USE_ASPELL_" = "true"), platforms = NULL,
  ...) {

  path <- normalizePath(path)
  assert_that(is_pkg_dir_or_tarball(path))

  platforms <- platforms %||% default_cran_check_platforms(path)

  check(path = path, platforms = platforms, email = email,
        check_args = check_args, env_vars = env_vars, ...)
}

default_cran_check_platforms <- function(path) {
  c(
    "windows-x86_64-devel",
    "ubuntu-gcc-release",
    "fedora-clang-devel",
    if (needs_compilation(path)) "linux-x86_64-rocker-gcc-san"
  )
}

## Various OSes --------------------------------------------------------

#' Check an R package on an R-hub platform
#'
#' These functions provide a quick easy to use interface to check a
#' package on a platform with some particular aspect. Which platform
#' they use might change over time.
#'
#' @param ... Additional arguments are passed to [check()].
#' @return An [rhub_check] object.
#' @inheritParams check
#'
#' @export
#' @rdname check_shortcuts

check_on_linux <- function(path = ".", ...) {
  check(path = path, platforms = check_shortcut_platforms$linux, ...)
}

#' @export
#' @rdname check_shortcuts

check_on_windows <- function(path = ".", ...) {
  check(path = path, platforms = check_shortcut_platforms$windows, ...)
}

#' @export
#' @rdname check_shortcuts

check_on_macos <- function(path = ".", ...) {
  check(path = path, platforms = check_shortcut_platforms$macos, ...)
}

## Various Linux OSes --------------------------------------------------

#' @export
#' @rdname check_shortcuts

check_on_debian <- function(path = ".", ...) {
  check(path = path, platforms = check_shortcut_platforms$debian, ...)
}

#' @export
#' @rdname check_shortcuts

check_on_ubuntu <- function(path = ".", ...) {
  check(path = path, platforms = check_shortcut_platforms$ubuntu, ...)
}

#' @export
#' @rdname check_shortcuts

check_on_fedora <- function(path = ".", ...) {
  check(path = path, platforms = check_shortcut_platforms$fedora, ...)
}

#' @export
#' @rdname check_shortcuts

check_on_solaris <- function(path = ".", check_args =
                             "'--no-manual --no-build-vignettes'", ...) {
  check(path = path, platforms = check_shortcut_platforms$solaris,
        check_args = check_args, ...)
}

#' @export
#' @rdname check_shortcuts

check_on_centos <- function(path = ".", ...) {
  check(path = path, platforms = check_shortcut_platforms$centos, ...)
}

## R versions --------------------------------------------------------

#' @export
#' @rdname check_shortcuts

check_with_roldrel <- function(path = ".", ...) {
  check(path = path, platforms = check_shortcut_platforms$roldrel, ...)
}

#' @export
#' @rdname check_shortcuts

check_with_rrelease <- function(path = ".", ...) {
  check(path = path, platforms = check_shortcut_platforms$rrelease, ...)
}

#' @export
#' @rdname check_shortcuts

check_with_rpatched <- function(path = ".", ...) {
  check(path = path, platforms = check_shortcut_platforms$rpatched, ...)
}

#' @export
#' @rdname check_shortcuts

check_with_rdevel <- function(path = ".", ...) {
  check(path = path, platforms = check_shortcut_platforms$rdevel, ...)
}

## Extra checks --------------------------------------------------------

#' @export
#' @rdname check_shortcuts

check_with_valgrind <- function(path = ".", ...) {
  check(path = path, platforms = check_shortcut_platforms$valgrind,
        valgrind = TRUE, ...)
}

#' @export
#' @rdname check_shortcuts

check_with_sanitizers <- function(path = ".", ...) {
  check(path = path, platforms = check_shortcut_platforms$sanitizers, ...)
}

## -------------------------------------------------------------------


check_shortcut_platforms <- list(
  "linux"      = "debian-gcc-release",
  "windows"    = "windows-x86_64-release",
  "macos"      = "macos-highsierra-release",
  "valgrind"   = "debian-gcc-release",
  "sanitizers" = "linux-x86_64-rocker-gcc-san",
  "roldrel"    = "windows-x86_64-oldrel",
  "rrelease"   = "debian-gcc-release",
  "rpatched"   = "debian-gcc-patched",
  "rdevel"     = "debian-gcc-devel",
  "debian"     = "debian-gcc-release",
  "ubuntu"     = "ubuntu-gcc-release",
  "fedora"     = "fedora-gcc-devel",
  "centos"     = "linux-x86_64-centos6-epel",
  "solaris"    = "solaris-x86-patched"
)

#' Check an R package on R-hub
#'
#' @param path Path to a directory containing an R package, or path to
#'   source R package tarball built with `R CMD build` or
#'   `devtools::build()`.
#' @param platforms A character vector of one or more platforms to build/check
#'   the package on. See [platforms()] for the available platforms. If this is
#'   \code{NULL}, and the R session is interactive, then a menu is shown. If it
#'   is \code{NULL}, and the session is not interactive, then the default R-hub
#'   platforms are used. A vector of platforms which saves time by building one
#'   R package tarball that is used for all the platforms specified.
#' @param email Email address to send notification to about the check.
#'   It must be a validated email address, see [validate_email()]. If
#'   `NULL`, then the email address of the maintainer is used, as defined
#'   in the `DESCRIPTION` file of the package.
#' @param valgrind Whether to run the check in valgrind. Only supported on
#'   Linux currently, and ignored on other platforms.
#' @param check_args Extra arguments for the `R CMD check` command.
#' @param env_vars Environment variables to set on the builder machine
#'   before the check. A named character vector.
#' @param show_status Whether to show the status of the build and check 
#' (live log) as it is happening.
#' @return An [rhub_check] object.
#'
#' @export
#' @examples
#' \dontrun{
#' check(".")
#' check("mypackage_1.0.0.tar.gz", platforms = "fedora-clang-devel")
#' }

check <- function(path = ".", platforms = NULL,
                  email = NULL, valgrind = FALSE, check_args = character(),
                  env_vars = character(), show_status = interactive()) {

  ## Check that it is a package
  path <- normalizePath(path)
  assert_that(is_pkg_dir_or_tarball(path))
  assert_that(is_flag(valgrind))
  assert_that(is_named(env_vars))
  assert_that(is.character(env_vars))

  ## Make sure that maintainer email was validated
  if (is.null(email)) email <- get_maintainer_email(path)
  if (is.na(email)) stop("Cannot get email address from package")
  assert_validated_email_for_check(email)

  platforms <- match_platform(platforms)

  ## Build the tar.gz, if needed
  if (file.info(path)$isdir) {
    header_line("Building package")
    pkg_targz <- build_package(path, tmpdir <- tempfile())
  } else {
    pkg_targz <- path
  }

  ## Add valgrind to check_args
  check_args <- c(
    check_args,
    if (valgrind) "--use-valgrind"
  )

  ## Submit to R-hub
  response <- submit_package(
    email,
    pkg_targz,
    platforms = platforms,
    check_args = check_args,
    env_vars = env_vars
  )

  ids <- vapply(response, "[[", "", "id")
  chk <- rhub_check$new(ids = ids)

  package_data$last_handle <- chk
  lapply(ids, cache_put, status = NULL)

  ## Show the live status, if requested
  if (show_status) chk$livelog()

  invisible(chk)
}

assert_validated_email_for_check <- function(email) {

  assert_that(is_email(email))
  code <- email_get_token(email)
  if (is.null(code)) {
    if (is_interactive()) {
      cat("\n")
      message(paste(collapse = "\n", strwrap(indent = 2, exdent = 2, paste0(
        sQuote(crayon::green(email)), " is not validated, or does not match ",
        "the package maintainer's email. To validate it now, please enter ",
        "the email address below. Note that R-hub will send a token to ",
        "this address. If the address does not belong to you, quit now by ",
        "pressing ", crayon::yellow("ENTER"), ". You can also specify a ",
        "different email by suppling email=."
      ))))
      cat("\n")
      email2 <- readline("  Email address: ")
      cat("\n")
      if (email2 == "") {
        stop("Aborting.", call. = FALSE)
      } else if (email != email2) {
        stop("Emails don't match, aborting", call. = FALSE)
      }
      validate_email(email)
    } else {
      stop(sQuote(email), " is not validated")
    }
  }
}

column_dt <- function(x) {
  as.difftime(x / 1000, units = "secs")
}

#' @importFrom pillar pillar_shaft new_pillar_shaft_simple
#' @importFrom prettyunits pretty_dt
#' @export

pillar_shaft.difftime <- function(x, ...) {
  cx <- my_pretty_dt(x)
  new_pillar_shaft_simple(cx, ...)
}

column_group_id <- function(x) {
  structure(x, class = unique(c("rhub_column_group_id", class(x))))
}

#' @export

`[.rhub_column_group_id` <- function(x, i) {
  column_group_id(NextMethod("["))
}

#' @importFrom pillar pillar_shaft new_pillar_shaft_simple
#' @export

pillar_shaft.rhub_column_group_id <- function(x, ...) {
  cx <- shorten_rhub_id(x)
  new_pillar_shaft_simple(cx, ...)
}

#' @importFrom pillar type_sum
#' @export

type_sum.rhub_column_group_id <- function(x) {
  "rhub::group_id"
}

column_id <- function(x) {
  structure(x, class = unique(c("rhub_column_id", class(x))))
}

#' @export

`[.rhub_column_id` <- function(x, i) {
  column_id(NextMethod("["))
}

#' @importFrom pillar pillar_shaft new_pillar_shaft_simple
#' @export

pillar_shaft.rhub_column_id <- function(x, ...) {
  cx <- shorten_rhub_id(x)
  new_pillar_shaft_simple(cx, ...)
}

#' @importFrom pillar type_sum
#' @export

type_sum.rhub_column_id <- function(x) {
  "rhub::id"
}
column_result <- function(x) {
  structure(x, class = unique(c("rhub_column_result", class(x))))
}

#' @export

`[.rhub_column_result` <- function(x, i) {
  column_result(NextMethod("["))
}

#' @importFrom pillar pillar_shaft new_pillar_shaft_simple
#' @export

pillar_shaft.rhub_column_result <- function(x, ...) {
  cx <- lapply(x, color_column_result)
  new_pillar_shaft_simple(cx, ...)
}

color_column_result <- function(x) {
  if (is.null(x)) return("in-progress")
  E <- if (n <- length(x$errors)) status_style_error(strrep("E", n))
  W <- if (n <- length(x$warnings)) status_style_error(strrep("W", n))
  N <- if (n <- length(x$notes)) status_style_note(strrep("N", n))

  switch(
    x$status,
    "parseerror" = status_style_error("parseerror"),
    "preperror" = status_style_error("preperror"),
    "aborted" = status_style_aborted("aborted"),
    "ok" = status_style_ok("ok"),
    paste0(E, W, N))
}

#' @importFrom pillar type_sum
#' @export

type_sum.rhub_column_result <- function(x) {
  "rhub::result"
}

column_status <- function(x) {
  structure(x, class = unique(c("rhub_column_status", class(x))))
}

#' @export

`[.rhub_column_status` <- function(x, i) {
  column_status(NextMethod("["))
}

#' @importFrom pillar pillar_shaft new_pillar_shaft_simple
#' @export

pillar_shaft.rhub_column_status <- function(x, ...) {
  ## status can be
  ## - created
  ## - in-progress
  ## - parseerror (the R-hub output parser failed)
  ## - preperror (build failed before R CMD check has started
  ## - aborted (build was aborted)
  ## - error
  ## - warning
  ## - note
  ## - ok

  hst <- c(
    "created"     = status_style_created("created"),
    "in-progress" = status_style_in_progress("in-progress"),
    "parseerror"  = status_style_error("parseerror"),
    "preperror"   = status_style_error("preperror"),
    "aborted"     = status_style_aborted("aborted"),
    "error"       = status_style_error("error"),
    "warning"     = status_style_error("warning"),
    "note"        = status_style_note("note"),
    "ok"          = status_style_ok("ok"))

  cx <- hst[x]
  cx[is.na(cx)] <- x[is.na(cx)]

  new_pillar_shaft_simple(cx, ...)
}

#' @importFrom pillar type_sum
#' @export

type_sum.rhub_column_status <- function(x) {
  "rhub::status"
}

#' Validate an email address on R-hub
#'
#' To build and check R packages on R-hub, you need to validate your
#' email address. This is because R-hub sends out emails about check
#' results.
#'
#' The `rhub` package stores validated email addresses in a user
#' configuration file, at a platform-dependent location.
#' On your current platform the file is at
#' \Sexpr[stage=render]{rhub:::email_file()}.
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
  token_file_msg()
  cat("\n")
  invisible()
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
      "Please check your emails for the R-hub access token."
    ))
    token <- readline("Token: ")
  }
  assert_that(is_token(token))

  ## We got everything now
  validate_email(email, token)
}

#' List validated email addresses
#'
#' @description List email addresses validated on R-hub on the current machine.
#'
#' @return A `data.frame` with two columns: `email` and `token`.
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
    if (is_interactive()) {
      token_file_msg()
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

token_file_msg <- function() {
  message(
    crayon::green(
      paste0(
        "For info the token(s) and email(s) are stored at ",
        email_file()
      )
    )
  )
}

package_data <- new.env(parent = emptyenv())
package_data$status <- new.env(parent = emptyenv())
package_data$ids <- character()
package_data$groups <- character()

## Since the status can be NULL, meaning unknown, we put all cache elements
## in a list of length 1.

cache_get <- function(id) {
  e <- package_data$status
  if (!is.null(x <- e[[id]][[1]])) return(x)
  nms <- ls(e)
  sts <- grep(paste0("-", id, "[0-9a-f]*$"), nms)
  if (length(sts) == 0) return(NULL)
  if (length(sts) == 1) return(e[[ nms[sts] ]][[1]])
  stop("Multiple checks match, please use a more specific id", call. = FALSE)
}

cache_put <- function(id, status) {
  cache_put_ids(id)
  cache_put_group_ids(status$group)
  package_data$status[[id]] <- list(status)
  invisible()
}

cache_put_ids <- function(id) {
  id <- unique(setdiff(id, package_data$ids))
  if (length(id)) package_data$ids <- c(id, package_data$ids)
}

cache_put_group_ids <- function(id) {
  id <- unique(setdiff(id, package_data$groups))
  if (length(id)) package_data$groups <- c(id, package_data$groups)
}

cache_get_ids <- function(ids) {
  w <- match_partial(ids, package_data$ids)
  package_data$ids[w]
}

cache_get_group_ids <- function(ids) {
  w <- match_partial(ids, package_data$groups)
  package_data$groups[w]
}

match_partial <- function(x, table) {
  hash <- sub("^.*-", "", table)
  m <- match(x, table)
  ifelse(is.na(m), pmatch(x, hash), m)
}

#' @importFrom crayon yellow red underline

report_system_error <- function(msg, status) {

  if (status$status == 0) return()

  if (status$stderr == "") {
    stop(
      msg, ", unknown error, standard output:\n",
      yellow(status$stdout),
      call. = FALSE
    )

  } else {
    stop(
      underline(yellow(paste0("\n", msg, ", standard output:\n\n"))),
      yellow(status$stdout), "\n",
      underline(red("Standard error:\n\n")), red(status$stderr),
      call. = FALSE
    )
  }
}

#' @importFrom httr status_code

report_error <- function(response) {
  if (status_code(response) < 300) {
    invisible(response)
  } else {
    call <- sys.call(-1)
    stop(create_condition(response, "error", call = call))
  }
}

#' @importFrom httr content

create_condition <- function(response,
                             class = c("error", "warning", "message"),
                             call) {

  class <- match.arg(class)

  message <- content(response)$message %||% "rhub error"

  structure(
    list(message = message, call = call),
    class = c("rhub_error", class, "condition")
  )
}

handle_id <- function(x) {
  if (is.character(x)) {
    unname(sub("^.*/([^/]+)$", "\\1", x, perl = TRUE))
  } else if (inherits(x, "rhub_handle")) {
    unname(vapply(x, "[[", "", "id"))
  } else {
    stop("Invalid R-hub check id")
  }
}

#' @export

print.rhub_handle <- function(x, ...) {
  id <- handle_id(x)
  if (length(id) == 1) {
    cat("R-hub check: ", id, "\n", sep = "")

  } else {
    cat(
      "R-hub checks:\n",
      paste("  ", id, collapse = "\n")
    )
  }
  invisible(x)
}

#' The last rhub check of this R session
#'
#' `rhub` caches the id(s) of the last submission. This can be retrieved
#' with `last_check`.
#'
#' @return An rhub_check object.
#'
#' @export
#' @examples
#' \dontrun{
#' check("packagedir")
#' last_check()
#' last_check()$livelog()
#' }

last_check <- function() {
  package_data$last_handle
}

#' List all checks for an email address
#'
#' @param email Email address. By default it is guessed with
#'   [whoami::email_address()]. The address must be validated, see
#'   [validate_email()].
#' @param package `NULL`, or a character scalar. Can be used to restrict
#'   the search for a single package.
#' @param howmany How many check groups (checks submitted simultaneously)
#'   to show. The current API limit is 20.
#' @return A [tibble::tibble] with columns:
#'   * package Name of the package.
#'   * version Package version.
#'   * result: More detailed result of the check. Can be `NULL` for errors.
#'     This is a list column with members: `status`, `errors`, `warnings`,
#'     `notes`.
#'   * group: R-hub check group id.
#'   * id: `R-hub check id.
#'   * platform_name: Name of the check platform.
#'   * build_time: Build time, a [difftime] object.
#'   * submitted: Time of submission.
#'   * started: Time of the check start.
#'   * platform: Detailed platform data, a list column.
#'   * builder: Name of the builder machine.
#'   * status Status of the check. Possible values:
#'     - `created`: check job was created, but not running yet.
#'     - `in-progress`: check job is running.
#'     - `parseerror`: internal R-hub error parsing the check results.
#'     - `preperror`: check error, before the package check has started.
#'     - `aborted`: aborted by admin or user.
#'     - `error`: failed check. (Possibly warnings and notes as well.)
#'     - `warning`: `R CMD check` reported warnings. (Possibly notes as well.)
#'     - `note`: `R CMD check` reported notes.
#'     - `ok`: successful check.
#'   * email: Email address of maintainer / submitter.
#'
#' @export
#' @seealso list_package_checks
#' @examples
#' \dontrun{
#' ch <- list_my_checks()
#' ch
#' ch$details()
#' }

list_my_checks <- function(email = email_address(), package = NULL,
                           howmany = 20) {

  assert_that(is_email(email))
  assert_that(is_string_or_null(package))
  assert_that(is_count(howmany))

  response <- if (is.null(package)) {
    query(
      "LIST BUILDS EMAIL",
      params = list(email = email, token = email_get_token(email)))
  } else {
    query(
      "LIST BUILDS PACKAGE",
      params = list(email = email, package = package,
                    token = email_get_token(email)))
  }

  if (length(response) > howmany) response <- response[seq_len(howmany)]

  make_check_list(response)
}


#' List checks of a package
#'
#' @param package Directory of an R package, or a package tarball.
#' @param email Email address that was used for the check(s).
#'   If `NULL`, then the maintainer address is used.
#' @param howmany How many checks to show. The current maximum of the API
#'   is 20.
#' @inherit list_my_checks return
#'
#' @export
#' @importFrom desc desc_get
#' @examples
#' \dontrun{
#' ch <- list_package_checks()
#' ch
#' ch$details(1)
#' }

list_package_checks <- function(package = ".", email = NULL, howmany = 20) {

  assert_that(is_pkg_dir_or_tarball(package))
  if (is.null(email)) email  <- get_maintainer_email(package)
  assert_that(is_email(email))
  assert_that(is_count(howmany))

  package <- unname(desc_get("Package", file = package))

  response <- query(
    "LIST BUILDS PACKAGE",
    params = list(email = email, package = package,
                  token = email_get_token(email))
  )

  if (length(response) > howmany) response <- response[seq_len(howmany)]

  make_check_list(response)
}

make_check_list <- function(response) {
  data <- unlist(response, recursive = FALSE)

  df <- tibble::tibble(
    package = map_chr(data, "[[", "package"),
    version = map_chr(data, "[[", "version"),
    result = column_result(map(data, function(x) x$result)),
    group = column_group_id(map_chr(data, "[[", "group")),
    id = column_id(map_chr(data, "[[", "id")),
    platform_name = map_chr(data, function(x) x$platform$name),
    build_time = column_dt(map_int(data, function(x) {
      suppressWarnings(as.integer(x$build_time %||% NA_integer_))
    })),
    submitted = column_time(map_chr(data, "[[", "submitted")),
    started = column_time(map_chr(data, function(x) x$started %||% NA_character_)),
    platform = map(data, "[[", "platform"),
    builder = map_chr(data, function(x) x$builder_machine %||% NA_character_),
    status = column_status(map_chr(data, "[[", "status")),
    email = map_chr(data, "[[", "email")
  )

  cache_put_ids(df$id)
  cache_put_group_ids(df$group)

  df
}

column_time <- function(x) {
  res <- rep(as.POSIXct(NA_character_), length(x))
  res[! is.na(x)] <- parse_iso_8601(x[!is.na(x)])
  res
}

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

#' Run a package check locally, in a Docker container
#'
#' @description Run a package check locally, in a Docker container. UNTESTED
#' ON WINDOWS, bug reports welcome. :-)
#'
#' @param quiet Whether to print the check output
#' @param image Docker image to use. If `NULL`, a default image is selected.
#' @param valgrind Whether to run the check with Valgrind.
#' @param timeout Timeout for a check, a `difftime` object or a scalar
#'   that will be interpreted as seconds.
#' @param artifacts Where to copy the build artifacts after the build.
#' @inheritParams check
#' @return An `rcmdcheck::rcmdcheck` object, with extra fields:
#'   * `all_output`: all output from the check, both standard output and
#'     error.
#'   * `container_name`: name of the Docker container that performed the
#'      build. It is a random name.
#'   * `artifacts`: directory of build artifacts.
#'
#' @export
#' @importFrom withr with_dir
#' @importFrom processx run
#' @importFrom utils tail
#' @importFrom uuid UUIDgenerate
#'
#' @details You'll need to have bash and Docker installed.

local_check_linux <- function(path = ".", quiet = FALSE, image = NULL,
      valgrind = FALSE, check_args = character(),
      env_vars = character(), timeout = Inf, artifacts = tempfile()) {

  ## Check that it is a package
  path <- normalizePath(path)
  assert_that(is_pkg_dir_or_tarball(path))
  assert_that(is_flag(quiet))
  assert_that(is.null(image) || is.character(image))
  assert_that(is_flag(valgrind))
  assert_that(is_named(env_vars))
  assert_that(is.character(env_vars))
  assert_that(is_timeout(timeout <- as_timeout(timeout)))
  assert_that(is.character(artifacts))

  if ((bash <- Sys.which("bash")) == "" || Sys.which("docker") == "") {
    stop("You need bash and Docker to run local Linux checks")
  }

  ## Build the tar.gz, if needed
  if (file.info(path)$isdir) {
    header_line("Building package")
    pkg_targz <- build_package(path, tmpdir <- tempfile())
  } else {
    pkg_targz <- path
  }

  ## Add valgrind to check_args
  check_args <- c(
    check_args,
    if (valgrind) "--use-valgrind"
  )

  dir.create(artifacts, showWarnings = FALSE, recursive = TRUE)
  artifacts <- normalizePath(artifacts)

  container_name <- UUIDgenerate()
  if (!quiet) {
    cat(sep = "", "\nContainer name: ", container_name, "-2", "\n")
    cat("It will _not_ be removed after the check.\n\n")
  }

  ## Arguments
  env_str <- paste(paste0(names(env_vars), "=", env_vars), collapse = "\n")
  args <- c(
    "-k",
    if (!is.null(image)) c("-i", image),
    if (length(check_args)) c("-c", paste(check_args, collapse = " ")),
    if (length(env_vars)) c("-e", env_str),
    c("-a", artifacts),
    c("-d", container_name),
    pkg_targz)

  output <- character()
  callback <- function(x, proc) output <<- c(output, x)

  ## Run it
  wd <- system.file(package = .packageName, "bin")
  result <- with_dir(
    wd,
    run(bash, c(file.path(wd, "rhub-linux.sh"), args), echo = TRUE,
        stdout_line_callback = callback, stderr_line_callback = callback,
        timeout = timeout, spinner = FALSE)
  )

  ## TODO: better error object
  if (result$timeout) stop("Check timed out")

  if (!quiet) cat("Artifacts in", artifacts, "\n")
  if (!quiet) cat(sep = "", "Container name: ", container_name, "-2", "\n\n")

  ## Try to parse as R CMD check result
  check_start <- grep("^>>>>>=====+ Running R CMD check", output)[1]
  if (is.na(check_start)) stop("Failed before check started")
  check_output <- tail(output, -check_start)
  check_result <- tryCatch(
    rcmdcheck::parse_check(text = check_output),
    error = function(e) NULL)

  result <- list(
    check_result = check_result,
    output = output,
    image = image,
    artifacts = artifacts,
    container_name = paste0(container_name, "-2"))
  class(result) <- "rhub_local_check"
  result
}

#' @importFrom utils head
#' @export

print.rhub_local_check <- function(x, ...) {
  cat0("<R-hub local check results>\n")
  if (!is.null(x$image)) cat0(symbol$bullet, " image: ", x$image, "\n")
  if (!is.null(x$output)) {
    cat0(symbol$bullet, " output:\n")
    cat(paste0("  ", c(head(x$output, 5), "...")), sep = "\n")
  }
  cat0(symbol$bullet, " container_name: ", x$container_name, "\n")
  if (!is.null(x$artifacts)) {
    cat0(symbol$bullet, " artifacts: \n  ", x$artifacts, "\n")
  }
  if (!is.null(x$check_result)) {
    cat0(symbol$bullet, " check_result:\n")
    print(x$check_result)
  }
}

#' List R-hub Docker images
#'
#' The images are pretty-printed in a short format. Use
#' `as.data.frame()` to get all available platform metadata.
#'
#' @export

local_check_linux_images <- function() {
  plat <- platforms()
  plat <- plat[!is.na(plat$`docker-image`), ]
  class(plat) <- c("rhub_docker_images", class(plat))
  plat
}

#' @export

print.rhub_docker_images <- function(x, ...) {
  res <- paste(cyan(paste0("rhub/", x$`docker-image`)),
               green(x$description), sep = ":\n  ")
  cat(res, sep = "\n")
  invisible(x)
}

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

check_print <- function(self, private) {
  self$update()
  for (x in private$status_) check_print2(x)
  invisible(self)
}

#' @importFrom parsedate parse_iso_8601
#' @importFrom prettyunits pretty_ms
#' @importFrom crayon make_style red yellow

check_print2 <- function(x) {

  title_line(paste0(x$package, " ", x$version, ": ", toupper(x$status)))

  greyish <- make_style("darkgrey")

  submitted_time <- as.numeric(Sys.time() - parse_iso_8601(x$submitted), units = "secs")
  submitted <- if (submitted_time > 0) {
    paste(pretty_ms(submitted_time * 1000), "ago")
  } else {
    "just now"
  }

  build_time <- if (!is.null(x$build_time) && x$build_time != 0) {
    paste0(greyish("  Build time: "), pretty_ms(x$build_time), "\n")
  }

  cat(
    sep = "",
    greyish("  Build ID:   "), x$id, "\n",
    greyish("  Platform:   "), x$platform$description, "\n",
    greyish("  Submitted:  "), submitted, "\n",
    build_time %||% "",
    "\n"
  )

  ## If not done, then this is all we do
  if (is.null(build_time)) return(invisible(x))

  ## R CMD check error
  if (tolower(x$status) != "preperror" && tolower(x$status) != "aborted") {
    makeshift <- structure(
      list(
        package = x$package,
        version = x$version,
        rversion = x$platform$rversion,
        output = list(stdout = x$check_output, stderr = "", status = 0),
        platform = x$platform$name,
        notes = x$result$notes,
        warnings = x$result$warnings,
        errors = x$result$errors
      ),
      class = "rcmdcheck"
    )
    print(makeshift, header = FALSE)

  ## Or we never got to R CMD check
  } else {
    clog <- gsub("+R-HUB-R-HUB-R-HUB", "", fixed = TRUE, x$preperror_log)
    clog <- gsub("\necho >>>>>=========[^\n]*\n", "\n", clog)
    clog <- gsub(
      "\n>>>>>=======* (.+)\n",
      yellow(sep = "", "\n\n", symbol$line, " \\1\n\n"),
      clog,
      perl = TRUE
    )

    cat(red(paste0(symbol$pointer, " Build failed during preparation or aborted\n")))
    cat(greyish("\n[...]\n"))
    cat(greyish(clog))
    cat("\n")
  }

  invisible(x)
}

#' @importFrom crayon make_style
#' @importFrom cli symbol

header_line <- function(x) {

  greyish <- make_style("darkgrey")

  cat(
    paste0("\r", greyish(symbol$line), "  "),
    greyish(x),
    "\n",
    sep = ""
  )
}

#' @importFrom crayon yellow
#' @importFrom cli symbol

title_line <- function(x) {

  cat(
    sep ="",
    "\n",
    yellow(paste0(symbol$line, symbol$line, " ", x)),
    "\n\n"
  )
}

#' @importFrom cli make_ansi_style style_bold style_inverse
#'   col_red col_blue col_green

status_style_created <- function(x) {
  x
}

status_style_in_progress <- function(x) {
  x
}

status_style_error <- function(x) {
  style_inverse(style_bold(col_red(x)))
}

status_style_aborted <- function(x) {
  style_bold(col_blue(x))
}

status_style_note <- function(x) {
  orange <- make_ansi_style("orange")
  style_bold(orange(x))
}

status_style_ok <- function(x) {
  style_inverse(style_bold(col_green(x)))
}

re_match_all <- function(text, pattern, ...) {

  text <- as.character(text)
  stopifnot(is.character(pattern), length(pattern) == 1, !is.na(pattern))

  ## Need to handle this case separately, as gregexpr effectively
  ## does not work for this.
  if (length(text) == 0) return(empty_result(text, pattern, ...))

  match <- gregexpr(pattern, text, perl = TRUE, ...)

  num_groups <- length(attr(match[[1]], "capture.names"))

  ## Non-matching strings have a rather strange special form,
  ## so we just treat them differently
  non <- vapply(match, function(m) m[1] == -1, TRUE)
  yes <- !non
  res <- replicate(length(text), list(), simplify = FALSE)
  if (any(non)) {
    res[non] <- list(replicate(num_groups + 1, character(), simplify = FALSE))
  }
  if (any(yes)) {
    res[yes] <- mapply(match1, text[yes], match[yes], SIMPLIFY = FALSE)
  }

  ## Need to assemble the final data frame "manually".
  ## There is apparently no function for this. rbind() is almost
  ## good, but simplifies to a matrix if the dimensions allow it....
  res <- lapply(seq_along(res[[1]]), function(i) {
    lapply(res, "[[", i)
  })

  structure(
    res,
    names = c(attr(match[[1]], "capture.names"), ".match"),
    row.names = seq_along(text),
    class = c("data.frame")
  )
}

match1 <- function(text1, match1) {

  matchstr <- substring(
    text1,
    match1,
    match1 + attr(match1, "match.length") - 1L
  )

  ## substring fails if the index is length zero,
  ## need to handle special case
  if (is.null(attr(match1, "capture.start"))) {
    list(.match = matchstr)

  } else {
    gstart  <- attr(match1, "capture.start")
    glength <- attr(match1, "capture.length")
    gend    <- gstart + glength - 1L

    groupstr <- substring(text1, gstart, gend)
    dim(groupstr) <- dim(gstart)

    c(lapply(seq_len(ncol(groupstr)), function(i) groupstr[, i]),
      list(.match = matchstr)
      )
  }
}

empty_result <- function(text, pattern, ...) {
  match <- regexpr(pattern, text, perl = TRUE, ...)
  num_groups <- length(attr(match, "capture.names"))
  structure(
    replicate(num_groups + 1, list(), simplify = FALSE),
    names = c(attr(match, "capture.names"), ".match"),
    row.names = integer(0),
    class = "data.frame"
  )
}
#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

#' @importFrom rematch re_match
#' @importFrom jsonlite base64_enc
#' @importFrom crayon blue

submit_package <- function(email, pkg_targz, platforms, check_args,
                           env_vars) {

  assert_that(is_email(email))
  assert_that(
    is.character(platforms),
    length(platforms) >= 1
  )

  m <- re_match(
    pattern = "^(?<package>.+)_(?<version>.+)\\.tar\\.gz",
    basename(pkg_targz)
  )

  header_line("Uploading package")
  buf <- readBin(pkg_targz, raw(), file.info(pkg_targz)$size)
  response <- query(
    "SUBMIT PACKAGE",
    data = list(
      email = unbox(email),
      token = unbox(email_get_token(email)),
      package = unbox(unname(m[, "package"])),
      version = unbox(unname(m[, "version"])),
      platform = platforms,
      env = as.list(env_vars),
      check_args = unbox(paste(check_args, collapse = " ")),
      file = unbox(base64_enc(buf))
    )
  )

  header_line(paste0(
    "Preparing build, see status at\n",
    blue(paste(
      "  ",
      vapply(response, "[[", "", "status-url"),
      collapse = "\n"
      ))
  ))

  response
}

update <- function(original, new) {

  if (length(new)) {
    if (length(original)) assert_that(is_named(original))
    assert_that(is_named(new))
    original[names(new)] <- new
  }

  original
}

#' @importFrom rematch re_match

parse_email <- function(x) {
  unname(
    re_match(pattern = "<(?<email>[^>]+)>", x)[, "email"]
  )
}

`%||%` <- function(l, r) if (is.null(l)) r else l

#' @importFrom desc desc_get_maintainer
#' @importFrom utils untar

get_maintainer_email <- function(path) {
  path <- normalizePath(path)
  if (is_dir(path)) {
    if (!file.exists(file.path(path, "DESCRIPTION"))) {
      stop("No 'DESCRIPTION' file found")
    }
    parse_email(desc_get_maintainer(path))
  } else {
    dir.create(tmp <- tempfile())
    files <- untar(path, list = TRUE, tar = "internal")
    desc <- grep("^[^/]+/DESCRIPTION$", files, value = TRUE)
    if (length(desc) < 1) stop("No 'DESCRIPTION' file in package")
    untar(path, desc, exdir = tmp, tar = "internal")
    parse_email(desc_get_maintainer(file.path(tmp, desc)))
  }
}

needs_compilation <- function(path) {
  path <- normalizePath(path)
  if (is_dir(path)) {
    file.exists(file.path(path, "src"))
  } else {
    dir.create(tmp <- tempfile())
    files <- untar(path, list = TRUE, tar = "internal")
    any(grepl("^[^/]+/src/?$", files))
  }
}

`%:::%` <- function(p, f) {
  get(f, envir = asNamespace(p))
}

is_interactive <- function() {
  interactive()
}

is_dir <- function(x) {
  file.info(x)$isdir
}

data_frame <- function(...) {
  data.frame(stringsAsFactors = FALSE, ...)
}

drop_nulls <- function(x) {
  x [ ! vapply(x, is.null, TRUE) ]
}

get_group <- function(l){
  if (! "group" %in% names(l)){
    ""
  } else {
    l[["group"]]
  }
}

cat0 <- function(..., sep = "") {
  cat(..., sep = sep)
}

map <- function(.x, .f, ...) {
  lapply(.x, .f, ...)
}

map_lgl <- function(.x, .f, ...) {
  vapply(.x, .f, logical(1), ...)
}

map_chr <- function(.x, .f, ...) {
  vapply(.x, .f, character(1), ...)
}

map_int <- function(.x, .f, ...) {
  vapply(.x, .f, integer(1), ...)
}

shorten_rhub_id <- function(x) {
  sx <- strsplit(x, "-", fixed = TRUE)
  substr(map_chr(sx, tail, 1), 1, 7)
}

## This is a workaround to handle NAs

my_pretty_dt <- function(x, compact = TRUE) {
  res <- rep("?", length(x))
  res[!is.na(x)] <- pretty_dt(x[!is.na(x)], compact = compact)
  res
}

problem_statuses <- function(){
  c("parseerror", "preperror", "aborted")
}
