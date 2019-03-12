
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rhub <img src='man/figures/logo.png' align="right" height="138.5" />

> Connect to R-hub, from R

<!-- badges: start -->

[![Linux Build
Status](https://travis-ci.org/r-hub/rhub.svg?branch=master)](https://travis-ci.org/r-hub/rhub)
[![Windows Build
status](https://ci.appveyor.com/api/projects/status/github/r-hub/rhub?svg=true)](https://ci.appveyor.com/project/gaborcsardi/rhub)
[![](http://www.r-pkg.org/badges/version/rhub)](http://www.r-pkg.org/pkg/rhub)
[![CRAN RStudio mirror
downloads](http://cranlogs.r-pkg.org/badges/rhub)](http://www.r-pkg.org/pkg/rhub)
[![Coverage
Status](https://img.shields.io/codecov/c/github/r-hub/rhub/master.svg)](https://codecov.io/github/r-hub/rhub?branch=master)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

Run `R CMD check` on any of the R-hub builder architectures, from R.

## Introduction

The [R-hub builder](https://builder.r-hub.io/) is a multi-platform build
and check service for R packages. This packages uses the R-hub API to
connect to the R-hub builder and start package checks on various
architectures.

It also supports accessing statuses of previous checks, and local use of
the R-hub Linux platforms via Docker.

## Installation

Install the package from CRAN:

``` r
install.packages("rhub")
```

Or get the development version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("r-hub/rhub")
```

## Basic usage

``` r
library(rhub)
```

### Validating your email address

To build packages, first you need to validate your email address with
`validate_email()`. The package tries to detect your email address using
[`whoami`](https://github.com/r-lib/whoami#whoami) and the maintainer
email listed in DESCRIPTION; and if it fails to do this correctly,
you’ll need to specify it.

![](man/figures/email-validation.png)

`rhub` stores the token permanently on the machine, so you do not need
to validate your email again. You can also copy your token to other
machines: run `list_validated_emails()` to get the token, and use the
`validate_email()` function on the new machine, using both the `email`
and `token` arguments.

Currently you cannot use the token obtained via this function in the Web
UI of the R-hub builder.

### Running a package check

`check()` runs an `R CMD check` on the package in the specified
directory, or specified source R package tarball created by `R CMD
build` or `devtools::build()`. It defaults to the working directory.

``` r
check()
```

If the `platform` argument is NULL, and the R session is interactive,
then a menu is shown. If it is NULL, and the session is not interactive,
then the default R-hub platform `platforms()$name[1]`,
i.e. debian-gcc-devel, is used.

In interactive R sessions, the output of the check is printed to the
screen unless you set the `show_status` argument to `FALSE`. In all
cases, you will receive a notification email with results after the
build.

![](man/figures/check-output.gif)

### Listing R-hub platforms

You can run checks on any platform. You can use the platform ids (e.g.
`debian-gcc-devel`) to select between platforms. You should not run
checks on all platforms at once. If preparing for a CRAN submission, use
the shortcut function `check_for_cran()` that will submit your package
to a few recommended platforms. If looking for a platform with
particular characteristics, in particular to reproduce a result from
CRAN’s own platform, have a look at the R-hub platform
characteristics.

``` r
knitr::kable(platforms())
```

|    | name                           | description                                                       | cran-name                          | rversion  | os-type | cpu-type | os-info                                        | compilers                                                    | docker-image       | sysreqs-platform           | categories               | node-labels                          | output-parser | macos-version |
| -- | :----------------------------- | :---------------------------------------------------------------- | :--------------------------------- | :-------- | :------ | :------- | :--------------------------------------------- | :----------------------------------------------------------- | :----------------- | :------------------------- | :----------------------- | :----------------------------------- | :------------ | :------------ |
| 1  | debian-gcc-devel               | Debian Linux, R-devel, GCC                                        | r-devel-linux-x86\_64-debian-gcc   | r-devel   | Linux   | x86\_64  | Debian GNU/Linux testing                       | GCC 6.2.0 (Debian 6.2.0-6)                                   | debian-gcc-devel   | linux-x86\_64-debian-gcc   | Linux                    | linux                                | NA            | NA            |
| 3  | debian-gcc-patched             | Debian Linux, R-patched, GCC                                      | r-patched-linux-x86\_64            | r-patched | Linux   | x86\_64  | Debian GNU/Linux testing                       | GCC 6.2.0 (Debian 6.2.0-6)                                   | debian-gcc-patched | linux-x86\_64-debian-gcc   | Linux                    | linux                                | NA            | NA            |
| 2  | debian-gcc-release             | Debian Linux, R-release, GCC                                      | r-release-linux-x86\_64            | r-release | Linux   | x86\_64  | Debian GNU/Linux testing                       | GCC 6.2.0 (Debian 6.2.0-6)                                   | debian-gcc-release | linux-x86\_64-debian-gcc   | Linux                    | linux                                | NA            | NA            |
| 5  | fedora-clang-devel             | Fedora Linux, R-devel, clang, gfortran                            | r-devel-linux-x86\_64-fedora-clang | r-devel   | Linux   | x86\_64  | Fedora 24                                      | clang version 3.8.0; GNU Fortran 6.1.1                       | fedora-clang-devel | linux-x86\_64-fedora-clang | Linux                    | linux                                | NA            | NA            |
| 4  | fedora-gcc-devel               | Fedora Linux, R-devel, GCC                                        | r-devel-linux-x86\_64-fedora-gcc   | r-devel   | Linux   | x86\_64  | Fedora 24                                      | GCC 6.1.1                                                    | fedora-gcc-devel   | linux-x86\_64-fedora-gcc   | Linux                    | linux                                | NA            | NA            |
| 8  | linux-x86\_64-centos6-epel     | CentOS 6, stock R from EPEL                                       | NA                                 | r-release | Linux   | x86\_64  | CentOS 6                                       | GCC 4.4.x                                                    | centos6-epel       | linux-x86\_64-centos6-epel | Linux                    | linux                                | NA            | NA            |
| 9  | linux-x86\_64-centos6-epel-rdt | CentOS 6 with Redhat Developer Toolset, R from EPEL               | NA                                 | r-release | Linux   | x86\_64  | CentOS 6                                       | GCC 5.2.1                                                    | centos6-epel-rdt   | linux-x86\_64-centos6-epel | Linux                    | linux                                | NA            | NA            |
| 10 | linux-x86\_64-rocker-gcc-san   | Debian Linux, R-devel, GCC ASAN/UBSAN                             | NA                                 | r-devel   | Linux   | x86\_64  | Debian GNU/Linux testing                       | GCC 5.4.0 (Debian 5.4.0-4)                                   | rocker-gcc-san     | linux-x86\_64-debian-gcc   | Checks for compiled code | linux                                | sanitizers    | NA            |
| 17 | macos-elcapitan-release        | macOS 10.11 El Capitan, R-release (experimental)                  | r-release-osx-x86\_64              | r-release | macOS   | x86\_64  | Mac OS X 10.11.6 15G1217                       | Apple LLVM version 8.0 (clang-800.0.42.1); GNU Fortran 4.2.3 | NA                 | osx-x86\_64-clang          | macOS                    | c(“macos”, “elcapitan”, “r-release”) | NA            | elcapitan     |
| 16 | macos-mavericks-oldrel         | macOS 10.9 Mavericks, R-oldrel (experimental)                     | r-oldrel-osx-x86\_64               | r-oldrel  | macOS   | x86\_64  | Mac OS X 10.9.5 13F1911                        | Apple LLVM version 6.0 (clang-600.0.57); GNU Fortran 4.2.3   | NA                 | osx-x86\_64-clang          | macOS                    | c(“macos”, “mavericks”, “r-oldrel”)  | NA            | mavericks     |
| 19 | solaris-x86-patched            | Oracle Solaris 10, x86, 32 bit, R-patched (experimental)          | r-patched-solaris-x86              | r-patched | Solaris | x86\_64  | SunOS 5.10 Generic\_147148-26 i86pc i386 i86pc | GCC 5.2.0                                                    | NA                 | solaris-10                 | Solaris                  | solaris                              | NA            | NA            |
| 6  | ubuntu-gcc-devel               | Ubuntu Linux 16.04 LTS, R-devel, GCC                              | NA                                 | r-devel   | Linux   | x86\_64  | Ubuntu 16.04 LTS                               | GCC 5.3.1                                                    | ubuntu-gcc-devel   | linux-x86\_64-ubuntu-gcc   | Linux                    | linux                                | NA            | NA            |
| 7  | ubuntu-gcc-release             | Ubuntu Linux 16.04 LTS, R-release, GCC                            | NA                                 | r-release | Linux   | x86\_64  | Ubuntu 16.04 LTS                               | GCC 5.3.1                                                    | ubuntu-gcc-release | linux-x86\_64-ubuntu-gcc   | Linux                    | linux                                | NA            | NA            |
| 18 | ubuntu-rchk                    | Ubuntu Linux 16.04 LTS, R-devel with rchk                         | NA                                 | r-devel   | Linux   | x86\_64  | Ubuntu 16.04 LTS                               | clang 3.8.0-2ubuntu4                                         | ubuntu-rchk        | linux-x86\_64-ubuntu-gcc   | Checks for compiled code | linux                                | rchk          | NA            |
| 14 | windows-x86\_64-devel          | Windows Server 2008 R2 SP1, R-devel, 32/64 bit                    | r-devel-windows-ix86+x86\_64       | r-devel   | Windows | x86\_64  | Windows Server 2008 R2 SP1                     | GCC 4.9.3, Rtools 3.4                                        | NA                 | windows-2008               | Windows                  | c(“windows”, “rtools3”)              | NA            | NA            |
| 15 | windows-x86\_64-devel-rtools4  | Windows Server 2012, R-devel, Rtools4.0, 32/64 bit (experimental) | NA                                 | r-testing | Windows | x86\_64  | Windows Server 2012                            | Rtools 4.0                                                   | NA                 | windows-2012               | Windows                  | c(“windows”, “rtools4.0”)            | NA            | NA            |
| 11 | windows-x86\_64-oldrel         | Windows Server 2008 R2 SP1, R-oldrel, 32/64 bit                   | r-oldrel-windows-ix86+x86\_64      | r-oldrel  | Windows | x86\_64  | Windows Server 2008 R2 SP1                     | GCC 4.6.3, Rtools 3.3                                        | NA                 | windows-2008               | Windows                  | c(“windows”, “rtools3”)              | NA            | NA            |
| 13 | windows-x86\_64-patched        | Windows Server 2008 R2 SP1, R-patched, 32/64 bit                  | NA                                 | r-patched | Windows | x86\_64  | Windows Server 2008 R2 SP1                     | GCC 4.9.3, Rtools 3.4                                        | NA                 | windows-2008               | Windows                  | c(“windows”, “rtools3”)              | NA            | NA            |
| 12 | windows-x86\_64-release        | Windows Server 2008 R2 SP1, R-release, 32/64 bit                  | r-release-windows-ix86+x86\_64     | r-release | Windows | x86\_64  | Windows Server 2008 R2 SP1                     | GCC 4.9.3, Rtools 3.4                                        | NA                 | windows-2008               | Windows                  | c(“windows”, “rtools3”)              | NA            | NA            |

### Shortcuts for quick checks

These are quick shortcuts that select the right platform:

  - `check_on_linux()` and `check_on_windows()` select the operating
    system.
  - `check_on_debian()`, `check_on_ubuntu()`, `check_on_fedora()` and
    `check_on_centos()` select an appropriate Linux platform.
  - `check_with_roldrel()`, `check_with_rrelease()`,
    `check_with_rpatched()` and `check_with_rdevel()` select an R
    version.
  - `check_with_valgrind()` runs the build and check on Linux, in
    `valgrind` to find memory leaks and pointer errors.
  - `check_with_sanitizers()` runs all package package tests, examples
    and vignettes with Address Sanitizer and Undefined Behavior
    Sanitizer, see below.

### Shortcut for preparing a CRAN submission

`check_for_cran()`: Check an R-package on R-hub, for a CRAN submission.

### Sanitizers for compiled code

R-hub provides a special Docker image to run Address Sanitizer (ASAN)
and Undefined Behavior Sanitizer (UBSAN). This is based on the
`r-devel-san` image of the [Rocker
project](https://github.com/rocker-org/r-devel-san).

This image does not run `R CMD check` at all, but it runs

1.  package tests,
2.  all manual examples, and
3.  all vignette code

with ASAN and UBSAN enabled.

## License

MIT © R Consortium
