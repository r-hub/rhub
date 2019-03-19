# rhub <img src='man/figures/logo.png' align="right" height="138.5" />

> Connect to R-hub, from R

<!-- badges: start -->
[![Linux Build Status](https://travis-ci.org/r-hub/rhub.svg?branch=master)](https://travis-ci.org/r-hub/rhub)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/r-hub/rhub?svg=true)](https://ci.appveyor.com/project/gaborcsardi/rhub)
[![](http://www.r-pkg.org/badges/version/rhub)](http://www.r-pkg.org/pkg/rhub)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/rhub)](http://www.r-pkg.org/pkg/rhub)
[![Coverage Status](https://img.shields.io/codecov/c/github/r-hub/rhub/master.svg)](https://codecov.io/github/r-hub/rhub?branch=master)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Gitter chat](https://badges.gitter.im/gitterHQ/gitter.png)](https://gitter.im/r-hub/community)
<!-- badges: end -->

Run `R CMD check` on any of the R-hub builder architectures, from R.

## Introduction

The [R-hub builder](https://builder.r-hub.io/) is a multi-platform build and
check service for R packages. This packages uses the R-hub API to connect to
the R-hub builder and start **package checks on various architectures**.

It also supports accessing **statuses of previous checks**, and 
**local use of the R-hub Linux platforms via Docker**.

## Installation

Install the package from CRAN:

```r
install.packages("rhub")
```

Or get the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("r-hub/rhub")
```

## Usage

Refer to the [`pkgdown` website](https://r-hub.github.io/rhub/), in particular 
the ["Get started" vignette](https://r-hub.github.io/rhub/articles/rhub.html)

![recording of a check on a screen](https://r-hub.github.io/rhub/articles/figures/check-output.gif)

## License

MIT © R Consortium
