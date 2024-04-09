# get_platforms

    Code
      cli::hash_obj_sha1(plt[[1]])
    Output
      [1] "c98c9abd5bf37f3c6e63ede7dc475cc598dd716e"
    Code
      cli::hash_obj_sha1(plt[[2]])
    Output
      [1] "b638216bc57a197de6cbec7db245b42b09f4813f"

# rhub_platforms

    Code
      rhub_platforms()
    Output
      -- Virtual machines ---------------------------------------------------------
       1 [VM]  linux
         All R versions on GitHub Actions ubuntu-latest
       2 [VM]  macos
         All R versions on GitHub Actions macos-latest
       3 [VM]  macos-arm64
         All R versions on GitHub Actions macos-14
       4 [VM]  windows
         All R versions on GitHub Actions windows-latest
      
      -- Containers ---------------------------------------------------------------
       5 [CT]  atlas  [ATLAS]
         R Under development (unstable) (2024-04-08 r86370) on Fedora Linux 38 (Con...
         ghcr.io/r-hub/containers/atlas:latest
       6 [CT]  clang-asan  [asan, clang-ASAN, clang-UBSAN, ubsan]
         R Under development (unstable) (2024-04-08 r86370) on Ubuntu 22.04.4 LTS
         ghcr.io/r-hub/containers/clang-asan:latest
       7 [CT]  clang16  [clang16]
         R Under development (unstable) (2024-04-06 r86351) on Ubuntu 22.04.4 LTS
         ghcr.io/r-hub/containers/clang16:latest
       8 [CT]  clang17  [clang17]
         R Under development (unstable) (2024-04-06 r86351) on Ubuntu 22.04.4 LTS
         ghcr.io/r-hub/containers/clang17:latest
       9 [CT]  clang18  [clang18]
         R Under development (unstable) (2024-04-06 r86351) on Ubuntu 22.04.4 LTS
         ghcr.io/r-hub/containers/clang18:latest
      10 [CT]  donttest  [donttest]
         R Under development (unstable) (2024-04-06 r86351) on Ubuntu 22.04.4 LTS
         ghcr.io/r-hub/containers/donttest:latest
      11 [CT]  gcc13  [gcc13]
         R Under development (unstable) (2024-04-08 r86370) on Fedora Linux 38 (Con...
         ghcr.io/r-hub/containers/gcc13:latest
      12 [CT]  intel  [Intel]
         R Under development (unstable) (2024-04-08 r86370) on Fedora Linux 38 (Con...
         ghcr.io/r-hub/containers/intel:latest
      13 [CT]  mkl  [MKL]
         R Under development (unstable) (2024-04-08 r86370) on Fedora Linux 38 (Con...
         ghcr.io/r-hub/containers/mkl:latest
      14 [CT]  nold  [noLD]
         R Under development (unstable) (2024-04-08 r86370) on Ubuntu 22.04.4 LTS
         ghcr.io/r-hub/containers/nold:latest
      15 [CT]  nosuggests  [noSuggests]
         R Under development (unstable) (2024-04-08 r86370) on Fedora Linux 38 (Con...
         ghcr.io/r-hub/containers/nosuggests:latest
      16 [CT]  ubuntu-clang  [r-devel-linux-x86_64-debian-clang]
         R Under development (unstable) (2024-04-08 r86370) on Ubuntu 22.04.4 LTS
         ghcr.io/r-hub/containers/ubuntu-clang:latest
      17 [CT]  ubuntu-gcc12  [r-devel-linux-x86_64-debian-gcc]
         R Under development (unstable) (2024-04-08 r86370) on Ubuntu 22.04.4 LTS
         ghcr.io/r-hub/containers/ubuntu-gcc12:latest
      18 [CT]  ubuntu-next  [r-next, r-patched, r-patched-linux-x86_64]
         R version 4.4.0 alpha (2024-04-07 r86351) on Ubuntu 22.04.4 LTS
         ghcr.io/r-hub/containers/ubuntu-next:latest
      19 [CT]  ubuntu-release  [r-release, r-release-linux-x86_64, ubuntu]
         R version 4.3.3 (2024-02-29) on Ubuntu 22.04.4 LTS
         ghcr.io/r-hub/containers/ubuntu-release:latest
      20 [CT]  valgrind  [valgrind]
         R Under development (unstable) (2024-04-08 r86370) on Fedora Linux 38 (Con...
         ghcr.io/r-hub/containers/valgrind:latest

---

    Code
      rhub_platforms()[]
    Output
      # A data frame: 3 x 8
        name         aliases   type      os_type container github_os r_version os_name
      * <chr>        <list>    <chr>     <chr>   <chr>     <chr>     <chr>     <chr>  
      1 windows      <chr [6]> os        Windows <NA>      windows-~ *         <NA>   
      2 ubuntu-clang <chr [1]> container Linux   ghcr.io/~ <NA>      R Under ~ Ubuntu~
      3 ubuntu-gcc12 <chr [1]> container Linux   ghcr.io/~ <NA>      <NA>      <NA>   

# format.rhub_platforms

    Code
      print(plt)
    Output
      -- Virtual machines ---------------------------------------------------------
       1 [VM]  linux
         R 4.4.0
       2 [VM]  macos
         All R versions on GitHub Actions macos-latest
       3 [VM]  macos-arm64
         All R versions on GitHub Actions macos-14
       4 [VM]  windows
         All R versions on GitHub Actions windows-latest
      
      -- Containers ---------------------------------------------------------------
       5 [CT]  atlas  [ATLAS]
         R Under development (unstable) (2024-04-08 r86370) on Fedora Linux 38 (Con...
         ghcr.io/r-hub/containers/atlas:latest
       6 [CT]  clang-asan  [asan, clang-ASAN, clang-UBSAN, ubsan]
         R Under development (unstable) (2024-04-08 r86370) on Ubuntu 22.04.4 LTS
         ghcr.io/r-hub/containers/clang-asan:latest
       7 [CT]  clang16  [clang16]
         R Under development (unstable) (2024-04-06 r86351) on Ubuntu 22.04.4 LTS
         ghcr.io/r-hub/containers/clang16:latest
       8 [CT]  clang17  [clang17]
         R Under development (unstable) (2024-04-06 r86351) on Ubuntu 22.04.4 LTS
         ghcr.io/r-hub/containers/clang17:latest
       9 [CT]  clang18  [clang18]
         R Under development (unstable) (2024-04-06 r86351) on Ubuntu 22.04.4 LTS
         ghcr.io/r-hub/containers/clang18:latest
      10 [CT]  donttest  [donttest]
         R Under development (unstable) (2024-04-06 r86351) on Ubuntu 22.04.4 LTS
         ghcr.io/r-hub/containers/donttest:latest
      11 [CT]  gcc13  [gcc13]
         R Under development (unstable) (2024-04-08 r86370) on Fedora Linux 38 (Con...
         ghcr.io/r-hub/containers/gcc13:latest
      12 [CT]  intel  [Intel]
         R Under development (unstable) (2024-04-08 r86370) on Fedora Linux 38 (Con...
         ghcr.io/r-hub/containers/intel:latest
      13 [CT]  mkl  [MKL]
         R Under development (unstable) (2024-04-08 r86370) on Fedora Linux 38 (Con...
         ghcr.io/r-hub/containers/mkl:latest
      14 [CT]  nold  [noLD]
         R Under development (unstable) (2024-04-08 r86370) on Ubuntu 22.04.4 LTS
         ghcr.io/r-hub/containers/nold:latest
      15 [CT]  nosuggests  [noSuggests]
         R Under development (unstable) (2024-04-08 r86370) on Fedora Linux 38 (Con...
         ghcr.io/r-hub/containers/nosuggests:latest
      16 [CT]  ubuntu-clang  [r-devel-linux-x86_64-debian-clang]
         R Under development (unstable) (2024-04-08 r86370) on Ubuntu 22.04.4 LTS
         ghcr.io/r-hub/containers/ubuntu-clang:latest
      17 [CT]  ubuntu-gcc12  [r-devel-linux-x86_64-debian-gcc]
         R Under development (unstable) (2024-04-08 r86370) on Ubuntu 22.04.4 LTS
         ghcr.io/r-hub/containers/ubuntu-gcc12:latest
      18 [CT]  ubuntu-next  [r-next, r-patched, r-patched-linux-x86_64]
         R version 4.4.0 alpha (2024-04-07 r86351) on Ubuntu 22.04.4 LTS
         ghcr.io/r-hub/containers/ubuntu-next:latest
      19 [CT]  ubuntu-release  [r-release, r-release-linux-x86_64, ubuntu]
         R version 4.3.3 (2024-02-29) on Ubuntu 22.04.4 LTS
         ghcr.io/r-hub/containers/ubuntu-release:latest
      20 [CT]  valgrind
         R Under development (unstable) (2024-04-08 r86370) on Fedora Linux 38 (Con...
         ghcr.io/r-hub/containers/valgrind:latest

# summary.rhub_platforms

    Code
      summary(plt)
    Output
       1 [VM] linux          R-* (any version)                 ubuntu-latest on Git
       2 [VM] macos          R-* (any version)                 macos-latest on GitH
       3 [VM] macos-arm64    R-* (any version)                 macos-14 on GitHub
       4 [VM] windows        R-* (any version)                 windows-latest on Gi...
       5 [CT] atlas          R-devel (2024-04-08 r86370)       Fedora Linux 38 (Con...
       6 [CT] clang-asan     R-devel (2024-04-08 r86370)       Ubuntu 22.04.4 LTS
       7 [CT] clang16        R-devel (2024-04-06 r86351)       Ubuntu 22.04.4 LTS
       8 [CT] clang17        R-devel (2024-04-06 r86351)       Ubuntu 22.04.4 LTS
       9 [CT] clang18        R-devel (2024-04-06 r86351)       Ubuntu 22.04.4 LTS
      10 [CT] donttest       R-devel (2024-04-06 r86351)       Ubuntu 22.04.4 LTS
      11 [CT] gcc13          R-devel (2024-04-08 r86370)       Fedora Linux 38 (Con...
      12 [CT] intel          R-devel (2024-04-08 r86370)       Fedora Linux 38 (Con...
      13 [CT] mkl            R-devel (2024-04-08 r86370)       Fedora Linux 38 (Con...
      14 [CT] nold           R-devel (2024-04-08 r86370)       Ubuntu 22.04.4 LTS
      15 [CT] nosuggests     R-devel (2024-04-08 r86370)       Fedora Linux 38 (Con...
      16 [CT] ubuntu-clang   R-devel (2024-04-08 r86370)       Ubuntu 22.04.4 LTS
      17 [CT] ubuntu-gcc12   R-devel (2024-04-08 r86370)       Ubuntu 22.04.4 LTS
      18 [CT] ubuntu-next    R-4.4.0 alpha (2024-04-07 r86351) Ubuntu 22.04.4 LTS
      19 [CT] ubuntu-release R-4.3.3 (2024-02-29)              Ubuntu 22.04.4 LTS
      20 [CT] valgrind       R-devel (2024-04-08 r86370)       Fedora Linux 38 (Con...

# select_platforms

    Code
      select_platforms()
    Condition
      Error:
      ! Failed to download the list of R-hub platforms.
      i Make sure that you are online and Github is also online.
      Caused by error:
      ! Not Found (HTTP 404).

---

    Code
      select_platforms()
    Condition
      Error:
      ! `platforms` argument is missing for `rhub_check()`.
      i You need to specify `platforms` in non-interactive sessions

---

    Code
      select_platforms(c("linux", "clang18"))
    Output
      [1] "linux"   "clang18"

---

    Code
      select_platforms(c("linux", "clang18", "thisisnotit"))
    Condition
      Error:
      ! Unknown platform: "thisisnotit".
      i See `rhub::rhub_platforms()` for the list of platforms

---

    Code
      select_platforms()
    Message
      
      Available platforms (see `rhub::rhub_platforms()` for details):
      
       1 [VM] linux          R-* (any version)                 ubuntu-latest on Git
       2 [VM] macos          R-* (any version)                 macos-latest on GitH
       3 [VM] macos-arm64    R-* (any version)                 macos-14 on GitHub
       4 [VM] windows        R-* (any version)                 windows-latest on Gi...
       5 [CT] atlas          R-devel (2024-04-08 r86370)       Fedora Linux 38 (Con...
       6 [CT] clang-asan     R-devel (2024-04-08 r86370)       Ubuntu 22.04.4 LTS
       7 [CT] clang16        R-devel (2024-04-06 r86351)       Ubuntu 22.04.4 LTS
       8 [CT] clang17        R-devel (2024-04-06 r86351)       Ubuntu 22.04.4 LTS
       9 [CT] clang18        R-devel (2024-04-06 r86351)       Ubuntu 22.04.4 LTS
      10 [CT] donttest       R-devel (2024-04-06 r86351)       Ubuntu 22.04.4 LTS
      11 [CT] gcc13          R-devel (2024-04-08 r86370)       Fedora Linux 38 (Con...
      12 [CT] intel          R-devel (2024-04-08 r86370)       Fedora Linux 38 (Con...
      13 [CT] mkl            R-devel (2024-04-08 r86370)       Fedora Linux 38 (Con...
      14 [CT] nold           R-devel (2024-04-08 r86370)       Ubuntu 22.04.4 LTS
      15 [CT] nosuggests     R-devel (2024-04-08 r86370)       Fedora Linux 38 (Con...
      16 [CT] ubuntu-clang   R-devel (2024-04-08 r86370)       Ubuntu 22.04.4 LTS
      17 [CT] ubuntu-gcc12   R-devel (2024-04-08 r86370)       Ubuntu 22.04.4 LTS
      18 [CT] ubuntu-next    R-4.4.0 alpha (2024-04-07 r86351) Ubuntu 22.04.4 LTS
      19 [CT] ubuntu-release R-4.3.3 (2024-02-29)              Ubuntu 22.04.4 LTS
      20 [CT] valgrind       R-devel (2024-04-08 r86370)       Fedora Linux 38 (Con...
    Output
      
      Selection (comma separated numbers, 0 to cancel):    1, 3, 9
      [1] "linux"       "macos-arm64" "clang18"    

---

    Code
      select_platforms()
    Message
      
      Available platforms (see `rhub::rhub_platforms()` for details):
      
       1 [VM] linux          R-* (any version)                 ubuntu-latest on Git
       2 [VM] macos          R-* (any version)                 macos-latest on GitH
       3 [VM] macos-arm64    R-* (any version)                 macos-14 on GitHub
       4 [VM] windows        R-* (any version)                 windows-latest on Gi...
       5 [CT] atlas          R-devel (2024-04-08 r86370)       Fedora Linux 38 (Con...
       6 [CT] clang-asan     R-devel (2024-04-08 r86370)       Ubuntu 22.04.4 LTS
       7 [CT] clang16        R-devel (2024-04-06 r86351)       Ubuntu 22.04.4 LTS
       8 [CT] clang17        R-devel (2024-04-06 r86351)       Ubuntu 22.04.4 LTS
       9 [CT] clang18        R-devel (2024-04-06 r86351)       Ubuntu 22.04.4 LTS
      10 [CT] donttest       R-devel (2024-04-06 r86351)       Ubuntu 22.04.4 LTS
      11 [CT] gcc13          R-devel (2024-04-08 r86370)       Fedora Linux 38 (Con...
      12 [CT] intel          R-devel (2024-04-08 r86370)       Fedora Linux 38 (Con...
      13 [CT] mkl            R-devel (2024-04-08 r86370)       Fedora Linux 38 (Con...
      14 [CT] nold           R-devel (2024-04-08 r86370)       Ubuntu 22.04.4 LTS
      15 [CT] nosuggests     R-devel (2024-04-08 r86370)       Fedora Linux 38 (Con...
      16 [CT] ubuntu-clang   R-devel (2024-04-08 r86370)       Ubuntu 22.04.4 LTS
      17 [CT] ubuntu-gcc12   R-devel (2024-04-08 r86370)       Ubuntu 22.04.4 LTS
      18 [CT] ubuntu-next    R-4.4.0 alpha (2024-04-07 r86351) Ubuntu 22.04.4 LTS
      19 [CT] ubuntu-release R-4.3.3 (2024-02-29)              Ubuntu 22.04.4 LTS
      20 [CT] valgrind       R-devel (2024-04-08 r86370)       Fedora Linux 38 (Con...
    Output
      
      Selection (comma separated numbers, 0 to cancel): 0
    Condition
      Error:
      ! R-hub check cancelled

---

    Code
      select_platforms()
    Message
      
      Available platforms (see `rhub::rhub_platforms()` for details):
      
       1 [VM] linux          R-* (any version)                 ubuntu-latest on Git
       2 [VM] macos          R-* (any version)                 macos-latest on GitH
       3 [VM] macos-arm64    R-* (any version)                 macos-14 on GitHub
       4 [VM] windows        R-* (any version)                 windows-latest on Gi...
       5 [CT] atlas          R-devel (2024-04-08 r86370)       Fedora Linux 38 (Con...
       6 [CT] clang-asan     R-devel (2024-04-08 r86370)       Ubuntu 22.04.4 LTS
       7 [CT] clang16        R-devel (2024-04-06 r86351)       Ubuntu 22.04.4 LTS
       8 [CT] clang17        R-devel (2024-04-06 r86351)       Ubuntu 22.04.4 LTS
       9 [CT] clang18        R-devel (2024-04-06 r86351)       Ubuntu 22.04.4 LTS
      10 [CT] donttest       R-devel (2024-04-06 r86351)       Ubuntu 22.04.4 LTS
      11 [CT] gcc13          R-devel (2024-04-08 r86370)       Fedora Linux 38 (Con...
      12 [CT] intel          R-devel (2024-04-08 r86370)       Fedora Linux 38 (Con...
      13 [CT] mkl            R-devel (2024-04-08 r86370)       Fedora Linux 38 (Con...
      14 [CT] nold           R-devel (2024-04-08 r86370)       Ubuntu 22.04.4 LTS
      15 [CT] nosuggests     R-devel (2024-04-08 r86370)       Fedora Linux 38 (Con...
      16 [CT] ubuntu-clang   R-devel (2024-04-08 r86370)       Ubuntu 22.04.4 LTS
      17 [CT] ubuntu-gcc12   R-devel (2024-04-08 r86370)       Ubuntu 22.04.4 LTS
      18 [CT] ubuntu-next    R-4.4.0 alpha (2024-04-07 r86351) Ubuntu 22.04.4 LTS
      19 [CT] ubuntu-release R-4.3.3 (2024-02-29)              Ubuntu 22.04.4 LTS
      20 [CT] valgrind       R-devel (2024-04-08 r86370)       Fedora Linux 38 (Con...
    Output
      
      Selection (comma separated numbers, 0 to cancel): 10000
    Condition
      Error:
      ! Invalid platform number: "10000".

