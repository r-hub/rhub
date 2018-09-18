
## Various OSes --------------------------------------------------------

#' Check an R package on an r-hub platform
#'
#' These functions provide a quick easy to use interface to check a
#' package on a platform with some particular aspect. Which platform
#' they use might change over time.
#'
#' @param ... Additional arguments are passed to [check()].
#' @return An [rhub_check] object.
#' @inheritParams check
#'
#' @family shortcut
#' @export
#' @rdname check_shortcuts

check_on_linux <- function(path = ".", ...) {
  check(path = path, platform = check_shortcut_platforms$linux, ...)
}

#' @family shortcut
#' @export
#' @rdname check_shortcuts

check_on_windows <- function(path = ".", ...) {
  check(path = path, platform = check_shortcut_platforms$windows, ...)
}

#' @family shortcut
#' @export
#' @rdname check_shortcuts

check_on_macos <- function(path = ".", ...) {
  check(path = path, platform = check_shortcut_platforms$macos, ...)
}

## Various Linux OSes --------------------------------------------------

#' @family shortcut
#' @export
#' @rdname check_shortcuts

check_on_debian <- function(path = ".", ...) {
  check(path = path, platform = check_shortcut_platforms$debian, ...)
}

#' @family shortcut
#' @export
#' @rdname check_shortcuts

check_on_ubuntu <- function(path = ".", ...) {
  check(path = path, platform = check_shortcut_platforms$ubuntu, ...)
}

#' @family shortcut
#' @export
#' @rdname check_shortcuts

check_on_fedora <- function(path = ".", ...) {
  check(path = path, platform = check_shortcut_platforms$fedora, ...)
}

#' @family shortcut
#' @export
#' @rdname check_shortcuts

check_on_centos <- function(path = ".", ...) {
  check(path = path, platform = check_shortcut_platforms$centos, ...)
}

## R versions --------------------------------------------------------

#' @family shortcut
#' @export
#' @rdname check_shortcuts

check_with_roldrel <- function(path = ".", ...) {
  check(path = path, platform = check_shortcut_platforms$roldrel, ...)
}

#' @family shortcut
#' @export
#' @rdname check_shortcuts

check_with_rrelease <- function(path = ".", ...) {
  check(path = path, platform = check_shortcut_platforms$rrelease, ...)
}

#' @family shortcut
#' @export
#' @rdname check_shortcuts

check_with_rpatched <- function(path = ".", ...) {
  check(path = path, platform = check_shortcut_platforms$rpatched, ...)
}

#' @family shortcut
#' @export
#' @rdname check_shortcuts

check_with_rdevel <- function(path = ".", ...) {
  check(path = path, platform = check_shortcut_platforms$rdevel, ...)
}

## Extra checks --------------------------------------------------------

#' @family shortcut
#' @export
#' @rdname check_shortcuts

check_with_valgrind <- function(path = ".", ...) {
  check(path = path, platform = check_shortcut_platforms$valgrind,
        valgrind = TRUE, ...)
}

#' @family shortcut
#' @export
#' @rdname check_shortcuts

check_with_sanitizers <- function(path = ".", ...) {
  check(path = path, platform = check_shortcut_platforms$sanitizers, ...)
}

## -------------------------------------------------------------------


check_shortcut_platforms <- list(
  "linux"      = "debian-gcc-release",
  "windows"    = "windows-x86_64-release",
  "macos"      = "macos-elcapitan-release",
  "valgrind"   = "debian-gcc-release",
  "sanitizers" = "linux-x86_64-rocker-gcc-san",
  "roldrel"    = "windows-x86_64-oldrel",
  "rrelease"   = "debian-gcc-release",
  "rpatched"   = "debian-gcc-patched",
  "rdevel"     = "debian-gcc-devel",
  "debian"     = "debian-gcc-release",
  "ubuntu"     = "ubuntu-gcc-release",
  "fedora"     = "fedora-gcc-devel",
  "centos"     = "linux-x86_64-centos6-epel"
)
