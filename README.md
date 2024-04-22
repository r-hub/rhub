

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

<div class="asciicast"
style="color: #B9C0CB;font-family: 'Fira Code',Monaco,Consolas,Menlo,'Bitstream Vera Sans Mono','Powerline Symbols',monospace;line-height: 1.300000">

<pre>
## Setting up R-hub v2.                                                            
## <span style="color: #A8CC8C;">✔</span> Found R package at <span style="color: #71BEF2;">/private/tmp/cli</span>.                                          
## <span style="color: #A8CC8C;">✔</span> Found git repository at <span style="color: #71BEF2;">/private/tmp/cli</span>.                                     
## <span style="color: #A8CC8C;">✔</span> Created workflow file <span style="color: #71BEF2;">/private/tmp/cli/.github/workflows/rhub.yaml</span>.           
##                                                                                 
## Notes:                                                                          
## <span style="color: #66C2CD;">•</span> The workflow file must be added to the <span style="font-style: italic;">default</span> branch of the GitHub           
##   repository.                                                                   
## <span style="color: #66C2CD;">•</span> GitHub actions must be enabled for the repository. They are disabled for      
##   forked repositories by default.                                               
##                                                                                 
## Next steps:                                                                     
## <span style="color: #66C2CD;">•</span> Add the workflow file to git using `git add &lt;filename&gt;`.                      
## <span style="color: #66C2CD;">•</span> Commit it to git using `git commit`.                                          
## <span style="color: #66C2CD;">•</span> Push the commit to GitHub using `git push`.                                   
## <span style="color: #66C2CD;">•</span> Call `rhub2::rhub_doctor()` to check that you have set up R-hub correctly.    
## <span style="color: #66C2CD;">•</span> Call `rhub2::rhub_check()` to check your package.                             
</pre>

</div>

2.  Run `git commit` and `git push` to push the workflow file to GitHub.

3.  Run `rhub::rhub_doctor()` to check if everything is set up
    correctly:

``` r
rhub::rhub_doctor()
```

<div class="asciicast"
style="color: #B9C0CB;font-family: 'Fira Code',Monaco,Consolas,Menlo,'Bitstream Vera Sans Mono','Powerline Symbols',monospace;line-height: 1.300000">

<pre>
## <span style="color: #A8CC8C;">✔</span> Found R package at <span style="color: #71BEF2;">/private/tmp/cli</span>.                                          
## <span style="color: #A8CC8C;">✔</span> Found git repository at <span style="color: #71BEF2;">/private/tmp/cli</span>.                                     
## <span style="color: #A8CC8C;">✔</span> Found GitHub PAT.                                                             
## <span style="color: #A8CC8C;">✔</span> Found repository on GitHub at <span style="font-style: italic;color: #71BEF2;">&lt;https://github.com/r-lib/cli&gt;</span>.                 
## <span style="color: #A8CC8C;">✔</span> GitHub PAT has the right scopes.                                              
## <span style="color: #A8CC8C;">✔</span> Found R-hub workflow in default branch, and it is active.                     
## → WOOT! You are ready to run `rhub2::rhub_check()` on this package.             
</pre>

</div>

### Run checks

Use `rhub::rhub_platforms()` to get a list of supported platforms and
checks:

``` r
rhub::rhub_platforms()
```

<div class="asciicast"
style="color: #B9C0CB;font-family: 'Fira Code',Monaco,Consolas,Menlo,'Bitstream Vera Sans Mono','Powerline Symbols',monospace;line-height: 1.300000">

<pre>
## ── Virtual machines ─────────────────────────────────────────────────────────── 
##  1 [VM]  <span style="font-weight: bold;color: #71BEF2;">linux</span>                                                                  
## <span style="color: #adadad;">   All R versions on GitHub Actions ubuntu-latest</span>                               
##  2 [VM]  <span style="font-weight: bold;color: #71BEF2;">macos</span>                                                                  
## <span style="color: #adadad;">   All R versions on GitHub Actions macos-latest</span>                                
##  3 [VM]  <span style="font-weight: bold;color: #71BEF2;">macos-arm64</span>                                                            
## <span style="color: #adadad;">   All R versions on GitHub Actions macos-14</span>                                    
##  4 [VM]  <span style="font-weight: bold;color: #71BEF2;">windows</span>                                                                
## <span style="color: #adadad;">   All R versions on GitHub Actions windows-latest</span>                              
##                                                                                 
## ── Containers ───────────────────────────────────────────────────────────────── 
##  5 [CT]  <span style="font-weight: bold;color: #71BEF2;">atlas</span><span style="color: #adadad;">  [ATLAS]</span>                                                         
## <span style="color: #adadad;">   R Under development (unstable) (2024-03-13 r86113) on Fedora Linux 38 (Conta</span>…
## <span style="font-style: italic;color: #adadad;">   ghcr.io/r-hub/containers/atlas:latest</span>                                        
##  6 [CT]  <span style="font-weight: bold;color: #71BEF2;">clang-asan</span><span style="color: #adadad;">  [asan, clang-ASAN, clang-UBSAN, ubsan]</span>                     
## <span style="color: #adadad;">   R Under development (unstable) (2024-03-12 r86109) on Ubuntu 22.04.4 LTS</span>     
## <span style="font-style: italic;color: #adadad;">   ghcr.io/r-hub/containers/clang-asan:latest</span>                                   
##  7 [CT]  <span style="font-weight: bold;color: #71BEF2;">clang16</span><span style="color: #adadad;">  [clang16]</span>                                                     
## <span style="color: #adadad;">   R Under development (unstable) (2024-03-12 r86109) on Ubuntu 22.04.4 LTS</span>     
## <span style="font-style: italic;color: #adadad;">   ghcr.io/r-hub/containers/clang16:latest</span>                                      
##  8 [CT]  <span style="font-weight: bold;color: #71BEF2;">clang17</span><span style="color: #adadad;">  [clang17]</span>                                                     
## <span style="color: #adadad;">   R Under development (unstable) (2024-03-11 r86098) on Ubuntu 22.04.4 LTS</span>     
## <span style="font-style: italic;color: #adadad;">   ghcr.io/r-hub/containers/clang17:latest</span>                                      
##  9 [CT]  <span style="font-weight: bold;color: #71BEF2;">clang18</span><span style="color: #adadad;">  [clang18]</span>                                                     
## <span style="color: #adadad;">   R Under development (unstable) (2024-03-12 r86109) on Ubuntu 22.04.4 LTS</span>     
## <span style="font-style: italic;color: #adadad;">   ghcr.io/r-hub/containers/clang18:latest</span>                                      
## 10 [CT]  <span style="font-weight: bold;color: #71BEF2;">donttest</span><span style="color: #adadad;">  [donttest]</span>                                                   
## <span style="color: #adadad;">   R Under development (unstable) (2024-03-12 r86109) on Ubuntu 22.04.4 LTS</span>     
## <span style="font-style: italic;color: #adadad;">   ghcr.io/r-hub/containers/donttest:latest</span>                                     
## 11 [CT]  <span style="font-weight: bold;color: #71BEF2;">gcc13</span><span style="color: #adadad;">  [gcc13]</span>                                                         
## <span style="color: #adadad;">   R Under development (unstable) (2024-03-13 r86113) on Fedora Linux 38 (Conta</span>…
## <span style="font-style: italic;color: #adadad;">   ghcr.io/r-hub/containers/gcc13:latest</span>                                        
## 12 [CT]  <span style="font-weight: bold;color: #71BEF2;">intel</span><span style="color: #adadad;">  [Intel]</span>                                                         
## <span style="color: #adadad;">   R Under development (unstable) (2024-03-13 r86113) on Fedora Linux 38 (Conta</span>…
## <span style="font-style: italic;color: #adadad;">   ghcr.io/r-hub/containers/intel:latest</span>                                        
## 13 [CT]  <span style="font-weight: bold;color: #71BEF2;">mkl</span><span style="color: #adadad;">  [MKL]</span>                                                             
## <span style="color: #adadad;">   R Under development (unstable) (2024-03-13 r86113) on Fedora Linux 38 (Conta</span>…
## <span style="font-style: italic;color: #adadad;">   ghcr.io/r-hub/containers/mkl:latest</span>                                          
## 14 [CT]  <span style="font-weight: bold;color: #71BEF2;">nold</span><span style="color: #adadad;">  [noLD]</span>                                                           
## <span style="color: #adadad;">   R Under development (unstable) (2024-03-13 r86113) on Ubuntu 22.04.4 LTS</span>     
## <span style="font-style: italic;color: #adadad;">   ghcr.io/r-hub/containers/nold:latest</span>                                         
## 15 [CT]  <span style="font-weight: bold;color: #71BEF2;">nosuggests</span><span style="color: #adadad;">  [noSuggests]</span>                                               
## <span style="color: #adadad;">   R Under development (unstable) (2024-03-13 r86113) on Fedora Linux 38 (Conta</span>…
## <span style="font-style: italic;color: #adadad;">   ghcr.io/r-hub/containers/nosuggests:latest</span>                                   
## 16 [CT]  <span style="font-weight: bold;color: #71BEF2;">ubuntu-clang</span><span style="color: #adadad;">  [r-devel-linux-x86_64-debian-clang]</span>                      
## <span style="color: #adadad;">   R Under development (unstable) (2024-03-13 r86113) on Ubuntu 22.04.4 LTS</span>     
## <span style="font-style: italic;color: #adadad;">   ghcr.io/r-hub/containers/ubuntu-clang:latest</span>                                 
## 17 [CT]  <span style="font-weight: bold;color: #71BEF2;">ubuntu-gcc12</span><span style="color: #adadad;">  [r-devel-linux-x86_64-debian-gcc]</span>                        
## <span style="color: #adadad;">   R Under development (unstable) (2024-03-13 r86113) on Ubuntu 22.04.4 LTS</span>     
## <span style="font-style: italic;color: #adadad;">   ghcr.io/r-hub/containers/ubuntu-gcc12:latest</span>                                 
## 18 [CT]  <span style="font-weight: bold;color: #71BEF2;">ubuntu-next</span><span style="color: #adadad;">  [r-next, r-patched, r-patched-linux-x86_64]</span>               
## <span style="color: #adadad;">   R version 4.3.3 Patched (2024-02-29 r86113) on Ubuntu 22.04.4 LTS</span>            
## <span style="font-style: italic;color: #adadad;">   ghcr.io/r-hub/containers/ubuntu-next:latest</span>                                  
## 19 [CT]  <span style="font-weight: bold;color: #71BEF2;">ubuntu-release</span><span style="color: #adadad;">  [r-release, r-release-linux-x86_64, ubuntu]</span>            
## <span style="color: #adadad;">   R version 4.3.3 (2024-02-29) on Ubuntu 22.04.4 LTS</span>                           
## <span style="font-style: italic;color: #adadad;">   ghcr.io/r-hub/containers/ubuntu-release:latest</span>                               
## 20 [CT]  <span style="font-weight: bold;color: #71BEF2;">valgrind</span><span style="color: #adadad;">  [valgrind]</span>                                                   
## <span style="color: #adadad;">   R Under development (unstable) (2024-03-13 r86113) on Fedora Linux 38 (Conta</span>…
## <span style="font-style: italic;color: #adadad;">   ghcr.io/r-hub/containers/valgrind:latest</span>                                     
</pre>

</div>

Run `rhub::rhub_check()` to start R-hub v2 checks on GitHub Actions:

``` r
rhub::rhub_check()
```

<div class="asciicast"
style="color: #B9C0CB;font-family: 'Fira Code',Monaco,Consolas,Menlo,'Bitstream Vera Sans Mono','Powerline Symbols',monospace;line-height: 1.300000">

<pre>
## <span style="color: #A8CC8C;">✔</span> Found git repository at <span style="color: #71BEF2;">/private/tmp/cli</span>.                                     
## <span style="color: #A8CC8C;">✔</span> Found GitHub PAT.                                                             
##                                                                                 
## Available platforms (see `rhub2::rhub_platforms()` for details):                
##                                                                                 
##  1 [VM] <span style="font-weight: bold;color: #71BEF2;">linux</span>          R-* (any version)                     ubuntu-latest on G…
##  2 [VM] <span style="font-weight: bold;color: #71BEF2;">macos</span>          R-* (any version)                     macos-latest on Gi…
##  3 [VM] <span style="font-weight: bold;color: #71BEF2;">macos-arm64</span>    R-* (any version)                     macos-14 on GitHub 
##  4 [VM] <span style="font-weight: bold;color: #71BEF2;">windows</span>        R-* (any version)                     windows-latest on …
##  5 [CT] <span style="font-weight: bold;color: #71BEF2;">atlas</span>          R-devel (2024-03-13 r86113)           Fedora Linux 38 (C…
##  6 [CT] <span style="font-weight: bold;color: #71BEF2;">clang-asan</span>     R-devel (2024-03-12 r86109)           Ubuntu 22.04.4 LTS 
##  7 [CT] <span style="font-weight: bold;color: #71BEF2;">clang16</span>        R-devel (2024-03-12 r86109)           Ubuntu 22.04.4 LTS 
##  8 [CT] <span style="font-weight: bold;color: #71BEF2;">clang17</span>        R-devel (2024-03-11 r86098)           Ubuntu 22.04.4 LTS 
##  9 [CT] <span style="font-weight: bold;color: #71BEF2;">clang18</span>        R-devel (2024-03-12 r86109)           Ubuntu 22.04.4 LTS 
## 10 [CT] <span style="font-weight: bold;color: #71BEF2;">donttest</span>       R-devel (2024-03-12 r86109)           Ubuntu 22.04.4 LTS 
## 11 [CT] <span style="font-weight: bold;color: #71BEF2;">gcc13</span>          R-devel (2024-03-13 r86113)           Fedora Linux 38 (C…
## 12 [CT] <span style="font-weight: bold;color: #71BEF2;">intel</span>          R-devel (2024-03-13 r86113)           Fedora Linux 38 (C…
## 13 [CT] <span style="font-weight: bold;color: #71BEF2;">mkl</span>            R-devel (2024-03-13 r86113)           Fedora Linux 38 (C…
## 14 [CT] <span style="font-weight: bold;color: #71BEF2;">nold</span>           R-devel (2024-03-13 r86113)           Ubuntu 22.04.4 LTS 
## 15 [CT] <span style="font-weight: bold;color: #71BEF2;">nosuggests</span>     R-devel (2024-03-13 r86113)           Fedora Linux 38 (C…
## 16 [CT] <span style="font-weight: bold;color: #71BEF2;">ubuntu-clang</span>   R-devel (2024-03-13 r86113)           Ubuntu 22.04.4 LTS 
## 17 [CT] <span style="font-weight: bold;color: #71BEF2;">ubuntu-gcc12</span>   R-devel (2024-03-13 r86113)           Ubuntu 22.04.4 LTS 
## 18 [CT] <span style="font-weight: bold;color: #71BEF2;">ubuntu-next</span>    R-4.3.3 (patched) (2024-02-29 r86113) Ubuntu 22.04.4 LTS 
## 19 [CT] <span style="font-weight: bold;color: #71BEF2;">ubuntu-release</span> R-4.3.3 (2024-02-29)                  Ubuntu 22.04.4 LTS 
## 20 [CT] <span style="font-weight: bold;color: #71BEF2;">valgrind</span>       R-devel (2024-03-13 r86113)           Fedora Linux 38 (C…
##                                                                                 
## Selection (comma separated numbers, 0 to cancel): 1, 5                          
##                                                                                 
## <span style="color: #A8CC8C;">✔</span> Check started: linux, atlas (daft-acornwoodpecker).                           
##   See <span style="font-style: italic;color: #71BEF2;">&lt;https://github.com/r-lib/cli/actions&gt;</span> for live output!                   
</pre>

</div>

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
