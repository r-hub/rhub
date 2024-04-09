# check_rpkg_root

    Code
      check_rpkg_root("/pkg/root", "/git/root")
    Condition
      Error:
      ! R-hub currently requires that your R package is at the root of the git repository.
      i Your R package is at '/pkg/root'.
      i Your git repository root is at '/git/root'.

# rhub_setup

    Code
      rhub_setup()
    Message
      Setting up R-hub v2.
      > Is the current directory part of an R package?
      v Found R package at '<wd>'.
      > Is the current directory part of a git repository?
      v Found git repository at '<wd>'.
    Condition
      Error:
      ! Failed to download R-hub worflow file from GitHub.
      i URL: <http://127.0.0.1:<port>/badbadbad>.
      i HTTP status: 404.
      i Make sure that you are online and GitHub is up.

---

    Code
      rhub_setup()
    Message
      Setting up R-hub v2.
      > Is the current directory part of an R package?
      v Found R package at '<wd>'.
      > Is the current directory part of a git repository?
      v Found git repository at '<wd>'.
      v Created workflow file '<wd>/.github/workflows/rhub.yaml'.
      
      Notes:
      * The workflow file must be added to the default branch of the GitHub repository.
      * GitHub actions must be enabled for the repository. They are disabled for forked repositories by default.
      
      Next steps:
      * Add the workflow file to git using `git add <filename>`.
      * Commit it to git using `git commit`.
      * Push the commit to GitHub using `git push`.
      * Call `rhub::rhub_doctor()` to check that you have set up R-hub correctly.
      * Call `rhub::rhub_check()` to check your package.

---

    Code
      rhub_setup()
    Message
      Setting up R-hub v2.
      > Is the current directory part of an R package?
      v Found R package at '<wd>'.
      > Is the current directory part of a git repository?
      v Found git repository at '<wd>'.
      v Workflow file '<wd>/.github/workflows/rhub.yaml' already exists and it is current.
      
      Notes:
      * The workflow file must be added to the default branch of the GitHub repository.
      * GitHub actions must be enabled for the repository. They are disabled for forked repositories by default.
      
      Next steps:
      * Add the workflow file to git using `git add <filename>`.
      * Commit it to git using `git commit` (if not committed already).
      * Push the commit to GitHub using `git push` (if not pushed already).
      * Call `rhub::rhub_doctor()` to check that you have set up R-hub correctly.
      * Call `rhub::rhub_check()` to check your package.

---

    Code
      rhub_setup()
    Message
      Setting up R-hub v2.
      > Is the current directory part of an R package?
      v Found R package at '<wd>'.
      > Is the current directory part of a git repository?
      v Found git repository at '<wd>'.
    Condition
      Error:
      ! Workflow file already exists at '<wd>/.github/workflows/rhub.yaml'.
      i Use `overwrite = TRUE` for overwriting it.

---

    Code
      rhub_setup(overwrite = TRUE)
    Message
      Setting up R-hub v2.
      > Is the current directory part of an R package?
      v Found R package at '<wd>'.
      > Is the current directory part of a git repository?
      v Found git repository at '<wd>'.
      i Updated existing workflow file at '<wd>/.github/workflows/rhub.yaml', as requested
      
      Notes:
      * The workflow file must be added to the default branch of the GitHub repository.
      * GitHub actions must be enabled for the repository. They are disabled for forked repositories by default.
      
      Next steps:
      * Add the workflow file to git using `git add <filename>`.
      * Commit it to git using `git commit`.
      * Push the commit to GitHub using `git push`.
      * Call `rhub::rhub_doctor()` to check that you have set up R-hub correctly.
      * Call `rhub::rhub_check()` to check your package.

# setup_find_r_package

    Code
      setup_find_r_package()
    Message
      > Is the current directory part of an R package?
      v Found R package at '<wd>'.
    Output
      [1] "<wd>"

---

    Code
      setup_find_r_package()
    Message
      > Is the current directory part of an R package?
      x Is the current directory part of an R package?
      
    Condition
      Error:
      ! The current directory is not part of an R package.
      i You can create an R package in the current directory if you run `usethis::create_package('.')`.
      i Alternatively, if you want to use R-hub for a package that is already on GitHub, supply the `gh_url` argument to `rhub_setup()`.

# setup_find_git_root

    Code
      setup_find_git_root()
    Message
      > Is the current directory part of a git repository?
      x Is the current directory part of a git repository?
      
    Condition
      Error:
      ! The current R package is not in a git repository.
      i You can create a git repository for the current package or project if you run `usethis::use_git()`.
      i Alternatively, if you want to use R-hub for a package that is already on GitHub, supply the `gh_url` argument to `rhub_setup()`.

---

    Code
      setup_find_git_root()
    Message
      > Is the current directory part of a git repository?
      v Found git repository at '<wd>'.
    Output
      [1] "<wd>"

