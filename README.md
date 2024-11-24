

<!-- README.md is generated from README.Rmd. Please edit that file -->

# rhub

> R-hub v2

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/r-hub/rhub/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/r-hub/rhub/actions/workflows/R-CMD-check.yaml)
[![](https://www.r-pkg.org/badges/version/rhub)](https://www.r-pkg.org/pkg/rhub)
[![Codecov test coverage](https://codecov.io/gh/r-hub/rhub/branch/main/graph/badge.svg)](https://app.codecov.io/gh/r-hub/rhub?branch=main)
<!-- badges: end -->

R-hub v2 uses GitHub Actions to run `R CMD check` and similar package checks.
The rhub package helps you set up R-hub v2 for your R package, and start
running checks.

---

- [Installation](#installation)
- [Usage](#usage)
  - [Requirements](#requirements)
  - [Private repositories](#private-repositories)
  - [Setup](#setup)
  - [Run checks](#run-checks)
- [The R Consortium runners](#the-r-consortium-runners)
  - [Limitations of the R Consortium
    runners](#limitations-of-the-r-consortium-runners)
- [Code of Conduct](#code-of-conduct)
- [License](#license)

## Installation

Install rhub from CRAN:

``` r
pak::pkg_install("rhub")
```

## Usage

### Requirements

- A Github account.
- Your R package must be in a GitHub repository.
- You need a GitHub [Personal Access
  Token](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token).
  You can use the [gitcreds package](https://gitcreds.r-lib.org/) to add
  the token to the git credential store.

See the [R Consortium runners](#the-r-consortium-runners) section for
using rhub if your package is not on GitHub.

### Private repositories

rhub uses GitHub Actions, which is free for public repositories. For
private repositories you also get some minutes for free, depending on
the GitHub subscription you have. See [About billing for GitHub
Actions](https://docs.github.com/en/billing/managing-billing-for-github-actions/about-billing-for-github-actions)
for details.

### Setup

1.  Switch to the directory of your package, and call
    `rhub::rhub_setup()` to add the R-hub workflow file to your package.

``` r
rhub::rhub_setup()
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/rhub-setup-dark.svg">
<img src="man/figures/rhub-setup.svg" /> </picture>

2.  Run `git commit` and `git push` to push the workflow file to GitHub.

3.  Run `rhub::rhub_doctor()` to check if everything is set up
    correctly:

``` r
rhub::rhub_doctor()
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/rhub-doctor-dark.svg">
<img src="man/figures/rhub-doctor.svg" /> </picture>

### Run checks

Use `rhub::rhub_platforms()` to get a list of supported platforms and
checks:

``` r
rhub::rhub_platforms()
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/rhub-platforms-dark.svg">
<img src="man/figures/rhub-platforms.svg" /> </picture>

Run `rhub::rhub_check()` to start R-hub v2 checks on GitHub Actions:

``` r
rhub::rhub_check()
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/rhub-check-dark.svg">
<img src="man/figures/rhub-check.svg" /> </picture>

## The R Consortium runners

If you don’t want to put your package on GitHub, you can still use the
rhub package to run package checks on any supported platform using a
shared pool of runners in the <https://github.com/r-hub2> GitHub
organization.

The process is similar to the first version of R-hub:

- Set your working directory to the R package you want to check.

- Obtain a token from R-hub, to verify your email address:

      rc_new_token()

  (You do not need to do this, if you already submitted packages to a
  previous version of R-hub from the same machine, using the same email
  address. Call `rc_list_local_tokens()` to check if you already have
  tokens.)

- Submit a build with

      rc_submit()

- Select the platforms you want to use, and follow the instructions and
  the link provided to see your check results.

### Limitations of the R Consortium runners

- You package will be public for the world, and will be stored in the
  <https://github.com/r-hub2> organization. Your check output and
  results will be public for anyone with a GitHub account. If you want
  to keep your package private, you can put it in a private GitHub
  repository, and use the `rhub_setup()` and `rhub_check()` functions
  instead.
- The R Consortium runners are shared among all users, so you might need
  to wait for your builds to start.
- You have to wait at least five minutes between submissions with
  `rc_submit()`.
- Currently you need to create a GitHub account to see the check logs of
  your package. You don’t need a GitHub account to submit the checks.

To avoid these limitations (except for the need for a GitHub account),
put your package in a GitHub repository, and use the `rhub_setup()` and
`rhub_check()` functions instead of `rc_submit()` and the R Consortium
runners.

## Code of Conduct

Please note that the rhub package is released with a [Contributor Code
of Conduct](https://r-hub.github.io/rhub/dev/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.

## License

MIT © R Consortium
