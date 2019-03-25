
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

  if ((bash <- Sys.which("bash")) == "") {
    stop("You need bash, and Docker run local Linux checks")
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
