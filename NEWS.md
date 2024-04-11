# rhub 2.0.0

## R-hub v2

This is a completely new system, see `?rhubv2`  manual page or
the 'Getting started with R-hub v2' article at
https://r-hub.github.io/rhub to start.

Previous functions are now deprecated and defunct. They will be removed
in the next version of the package.

# rhub 1.1.2

* Replace `platform` parameter with `platforms` in `check()` (#497).

* Update shortcut for Mac OS platform (@echasnovski, #393)

* `cran_summary()` now works even if there is a NOTE/WARNING/ERROR in one
  platform and nothing on other platforms (@fabian-s, #259).

* Check results have now a `get_ids()` method, to easily query the id(s)
  of the check.

# rhub 1.1.1

## Enhancements

* `cran_summary()` now messages that we recommend to fix all NOTEs, WARNINGs
  and ERRORs before a CRAN submission when the check results aren't 0 NOTE, 0
  WARNING, 0 ERROR.

* `cran_summary()` now outputs informative messages when any of the builds
  of the group hasn't completed (yet, or at all).

## Bug fixes

* `cran_summary()` now works for packages whose R CMD Check result include
  no NOTE/WARNING/ERROR, and gives an informative error message when not all
  builds are completed yet.

* `cran_summary()` now prints lines to screen without unwanted indentation.

# rhub 1.1.0

## New features

* New `local_check_linux()` function to run an R-hub check on the local
  host's Docker. New `local_check_linux_images()` function to list R-hub
  Docker images.

* New `check_on_solaris()` shortcut to check on Solaris X86, without
  building the PDF manual or the vignettes.

* New `get_check()` function that works with check ids, or a check group id.

* `list_package_checks()` and `list_my_checks()` now output a `tibble`, that
  is nicely formatted when printed to the screen.

* The output of `get_check()`, `check()`, `check_on_`, `check_for_cran()`,
  etc. functions gained
    * an `urls()` method returning a `data.frame` with URLs to the html and
    text logs, as well as the artifacts, of the check(s);
    * a `browse()` method replacing the `web()` method for opening the
  URLs corresponding to a `rhub_check` object.

* New `cran_summary()` method to print a summary for a group or set of
  checks.

## Bug fixes

* In printing methods the submitted time is now always correct thanks to
  explicitly specifying units for `as.numeric.difftime` (@jimhester, #94
  and @schloerke, #135).

# rhub 1.0.2

First public release.
