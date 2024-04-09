# rhub_doctor

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
      rhub_doctor()
    Message
      > Is the current directory part of an R package?
      v Found R package at '<wd>'.
      > Is the current directory part of a git repository?
      v Found git repository at '<wd>'.
      > WOOT! You are ready to run `rhub::rhub_check()` on this package.

# doctor_find_gh_url

    Code
      doctor_find_gh_url(".")
    Condition
      Error:
      ! Cannot determine GitHub URL from git remote in repository at '.'. Is your repository on GitHub?
      i If this repository is on GitHub, call `git remote add origin <github-url>` to add GitHub as a remote.
      i Alternatively, specify the GitHub URL of the repository in the `gh_url` argument.
      i If it is not on GitHub, then you'll need to put it there. Create a new repository at <https://github.com/new>.

# doctor_find_pat

    Code
      doctor_find_pat("https://github.com")
    Message
      > Do you have a GitHub personal access token (PAT)?
      x Do you have a GitHub personal access token (PAT)?
      
    Condition
      Error:
      ! Could not find a GitHub personal access token (PAT) for <https://github.com>.
      i I also could not find a working git installation. If you don't want to install git, but you have a PAT, you can set the GITHUB_PAT_GITHUB_COM environment variable to the PAT.
      i You can read more about PATs at <https://usethis.r-lib.org/articles/git-credentials.html>.

---

    Code
      doctor_find_pat("https://github.com")
    Message
      > Do you have a GitHub personal access token (PAT)?
      x Do you have a GitHub personal access token (PAT)?
      
    Condition
      Error:
      ! Could not find a GitHub personal access token (PAT) for <https://github.com>.
      i If you have a GitHub PAT, you can use `gitcreds::gitcreds_set()` to add it to the git credential store, so R-hub can use it.
      i If you don't have a PAT, you can create one by running `usethis::create_github_token()`.
      i You can read more about PATs at <https://usethis.r-lib.org/articles/git-credentials.html>.

---

    Code
      doctor_find_pat("https://github.com")
    Message
      > Do you have a GitHub personal access token (PAT)?
      x Do you have a GitHub personal access token (PAT)?
      
    Condition
      Error:
      ! oops

---

    Code
      doctor_find_pat("https://github.com")
    Message
      > Do you have a GitHub personal access token (PAT)?
      v Found GitHub PAT.
    Output
      [1] "secret"

# doctor_check_github

    Code
      doctor_check_github("https://github.com/r-lib/ps", resp)
    Message
      > Is the package on GitHub at <https://github.com/r-lib/ps>?
      v Found repository on GitHub at <https://github.com/r-lib/ps>.

---

    Code
      doctor_check_github("https://github.com/r-lib/ps", resp2)
    Message
      > Is the package on GitHub at <https://github.com/r-lib/ps>?
      x Is the package on GitHub at <https://github.com/r-lib/ps>?
      
    Condition
      Error:
      ! Remote repository at <https://github.com/r-lib/ps> does not seem like a GitHub repository.
      i R-hub only supports GitHub packages in GitHub repositories currently.
      i If you think that this is a bug in the rhub package, please let us know!

# doctor_check_pat_scopes

    Code
      doctor_check_pat_scopes(resp)
    Message
      > Does your GitHub PAT have the right scopes?
      x Does your GitHub PAT have the right scopes?
      
    Condition
      Error:
      ! Could not use the PAT to authenticate to GitHub
      i Make sure that the URL and your PAT are correct.

---

    Code
      doctor_check_pat_scopes(resp2)
    Message
      > Does your GitHub PAT have the right scopes?
      x Does your GitHub PAT have the right scopes?
      
    Condition
      Error:
      ! Your PAT does not have a `repo` scope.
      i Withoput a `repo` scope R-hub cannot start jobs on GitHub.
      i Change the scopes of the PAT on the GitHub web page, or create a new PAT.

---

    Code
      doctor_check_pat_scopes(resp3)
    Message
      > Does your GitHub PAT have the right scopes?
      v GitHub PAT has the right scopes.

# doctor_check_workflow

    Code
      doctor_check_workflow(url, list(), list())
    Message
      > Does the default branch of your git repo have the R-hub workflow file?
      x Does the default branch of your git repo have the R-hub workflow file?
      
    Condition
      Error:
      ! Could not find R-hub's workflow file in the repository at <https://github.com/r-lib/ps>.
      i The workflow file must be at '.github/workflows/rhub.yaml'.
      i If you have added and committed the workflow file, you need to push the commit to GitHub with `git push`.

---

    Code
      doctor_check_workflow(url, list(is_forked = TRUE), list())
    Message
      > Does the default branch of your git repo have the R-hub workflow file?
      x Does the default branch of your git repo have the R-hub workflow file?
      
    Condition
      Error:
      ! Could not find R-hub's workflow file in the repository at <https://github.com/r-lib/ps>.
      i The workflow file must be at '.github/workflows/rhub.yaml'.
      i If you have added and committed the workflow file, you need to push the commit to GitHub with `git push`.
      i This repository is a fork. Make sure you enabled GitHub Actions on it, in the Actions tab of the repository web page.

---

    Code
      doctor_check_workflow(url, list(workflow = "ok"), list(workflow = list(state = "bad")))
    Message
      > Does the default branch of your git repo have the R-hub workflow file?
      x Does the default branch of your git repo have the R-hub workflow file?
      
    Condition
      Error:
      ! The workflow is disabled.
      i You need to enable it, click on the `...` button at the top right corner of the web page of the workflow.

---

    Code
      doctor_check_workflow(url, list(workflow = "ok"), list(workflow = list(state = "active")))
    Message
      > Does the default branch of your git repo have the R-hub workflow file?
      v Found R-hub workflow in default branch, and it is active.

# doctor_async_gql

    Code
      synchronise(doctor_async_gql("https://github.com/r-lib/ps", "secret"))
    Output
      $status_code
      [1] 200
      
      $headers
      $headers$server
      [1] "GitHub.com"
      
      $headers$date
      [1] "Tue, 09 Apr 2024 15:19:50 GMT"
      
      $headers$`content-type`
      [1] "application/json; charset=utf-8"
      
      $headers$`x-oauth-scopes`
      [1] "delete:packages, delete_repo, read:org, repo, workflow, write:packages"
      
      $headers$`x-accepted-oauth-scopes`
      [1] "repo"
      
      $headers$`x-github-media-type`
      [1] "github.v4; format=json"
      
      $headers$`x-ratelimit-limit`
      [1] "5000"
      
      $headers$`x-ratelimit-remaining`
      [1] "4999"
      
      $headers$`x-ratelimit-reset`
      [1] "1712679590"
      
      $headers$`x-ratelimit-used`
      [1] "1"
      
      $headers$`x-ratelimit-resource`
      [1] "graphql"
      
      $headers$`access-control-expose-headers`
      [1] "ETag, Link, Location, Retry-After, X-GitHub-OTP, X-RateLimit-Limit, X-RateLimit-Remaining, X-RateLimit-Used, X-RateLimit-Resource, X-RateLimit-Reset, X-OAuth-Scopes, X-Accepted-OAuth-Scopes, X-Poll-Interval, X-GitHub-Media-Type, X-GitHub-SSO, X-GitHub-Request-Id, Deprecation, Sunset"
      
      $headers$`access-control-allow-origin`
      [1] "*"
      
      $headers$`strict-transport-security`
      [1] "max-age=31536000; includeSubdomains; preload"
      
      $headers$`x-frame-options`
      [1] "deny"
      
      $headers$`x-content-type-options`
      [1] "nosniff"
      
      $headers$`x-xss-protection`
      [1] "0"
      
      $headers$`referrer-policy`
      [1] "origin-when-cross-origin, strict-origin-when-cross-origin"
      
      $headers$`content-security-policy`
      [1] "default-src 'none'"
      
      $headers$vary
      [1] "Accept-Encoding, Accept, X-Requested-With"
      
      $headers$`content-encoding`
      [1] "gzip"
      
      $headers$`x-github-request-id`
      [1] "F96D:2784C9:156C97B:157F2DD:66155C96"
      
      
      $is_repo
      [1] TRUE
      
      $workflow_binary
      [1] FALSE
      
      $workflow
      [1] "# R-hub's generic GitHub Actions workflow file. It's canonical location is at\n# https://github.com/r-hub/actions/blob/v1/workflows/rhub.yaml\n# You can update this file to a newer version using the rhub2 package:\n#\n# rhub::rhub_setup()\n#\n# It is unlikely that you need to modify this file manually.\n\nname: R-hub\nrun-name: \"${{ github.event.inputs.id }}: ${{ github.event.inputs.name || format('Manually run by {0}', github.triggering_actor) }}\"\n\non:\n  workflow_dispatch:\n    inputs:\n      config:\n        description: 'A comma separated list of R-hub platforms to use.'\n        type: string\n        default: 'linux,windows,macos'\n      name:\n        description: 'Run name. You can leave this empty now.'\n        type: string\n      id:\n        description: 'Unique ID. You can leave this empty now.'\n        type: string\n\njobs:\n\n  setup:\n    runs-on: ubuntu-latest\n    outputs:\n      containers: ${{ steps.rhub-setup.outputs.containers }}\n      platforms: ${{ steps.rhub-setup.outputs.platforms }}\n\n    steps:\n    # NO NEED TO CHECKOUT HERE\n    - uses: r-hub/actions/setup@main\n      with:\n        config: ${{ github.event.inputs.config }}\n      id: rhub-setup\n\n  linux-containers:\n    needs: setup\n    if: ${{ needs.setup.outputs.containers != '[]' }}\n    runs-on: ubuntu-latest\n    name: ${{ matrix.config.label }}\n    strategy:\n      fail-fast: false\n      matrix:\n        config: ${{ fromJson(needs.setup.outputs.containers) }}\n    container:\n      image: ${{ matrix.config.container }}\n\n    steps:\n      - uses: r-hub/actions/checkout@main\n      - uses: r-hub/actions/platform-info@main\n        with:\n          token: ${{ secrets.RHUB_TOKEN }}\n          job-config: ${{ matrix.config.job-config }}\n      - uses: r-hub/actions/setup-deps@main\n        with:\n          token: ${{ secrets.RHUB_TOKEN }}\n          job-config: ${{ matrix.config.job-config }}\n      - uses: r-hub/actions/run-check@main\n        with:\n          token: ${{ secrets.RHUB_TOKEN }}\n          job-config: ${{ matrix.config.job-config }}\n\n  other-platforms:\n    needs: setup\n    if: ${{ needs.setup.outputs.platforms != '[]' }}\n    runs-on: ${{ matrix.config.os }}\n    name: ${{ matrix.config.label }}\n    strategy:\n      fail-fast: false\n      matrix:\n        config: ${{ fromJson(needs.setup.outputs.platforms) }}\n\n    steps:\n      - uses: r-hub/actions/checkout@main\n      - uses: r-hub/actions/setup-r@main\n        with:\n          job-config: ${{ matrix.config.job-config }}\n          token: ${{ secrets.RHUB_TOKEN }}\n      - uses: r-hub/actions/platform-info@main\n        with:\n          token: ${{ secrets.RHUB_TOKEN }}\n          job-config: ${{ matrix.config.job-config }}\n      - uses: r-hub/actions/setup-deps@main\n        with:\n          job-config: ${{ matrix.config.job-config }}\n          token: ${{ secrets.RHUB_TOKEN }}\n      - uses: r-hub/actions/run-check@main\n        with:\n          job-config: ${{ matrix.config.job-config }}\n          token: ${{ secrets.RHUB_TOKEN }}\n"
      
      $sha
      [1] "1ee32843d8fd8dbae325ce50458e8eba96ab894f"
      
      $branch
      [1] "main"
      
      $is_fork
      [1] FALSE
      
      $errors
      NULL
      

# doctor_async_rest

    Code
      synchronise(doctor_async_rest("https://github.com/r-lib/ps", "secret"))
    Output
      $status_code
      [1] 200
      
      $headers
      $headers$server
      [1] "GitHub.com"
      
      $headers$date
      [1] "Tue, 09 Apr 2024 15:24:07 GMT"
      
      $headers$`content-type`
      [1] "application/json; charset=utf-8"
      
      $headers$`cache-control`
      [1] "private, max-age=60, s-maxage=60"
      
      $headers$vary
      [1] "Accept, Authorization, Cookie, X-GitHub-OTP"
      
      $headers$etag
      [1] "W/\"c65a62f3c04bdadcf817bd671991e78e3759e96c2894ab8a301a2e469c6a2ea6\""
      
      $headers$`x-oauth-scopes`
      [1] "delete:packages, delete_repo, read:org, repo, workflow, write:packages"
      
      $headers$`x-accepted-oauth-scopes`
      [1] ""
      
      $headers$`x-github-media-type`
      [1] "github.v3; format=json"
      
      $headers$`x-github-api-version-selected`
      [1] "2022-11-28"
      
      $headers$`x-ratelimit-limit`
      [1] "5000"
      
      $headers$`x-ratelimit-remaining`
      [1] "4975"
      
      $headers$`x-ratelimit-reset`
      [1] "1712676928"
      
      $headers$`x-ratelimit-used`
      [1] "25"
      
      $headers$`x-ratelimit-resource`
      [1] "core"
      
      $headers$`access-control-expose-headers`
      [1] "ETag, Link, Location, Retry-After, X-GitHub-OTP, X-RateLimit-Limit, X-RateLimit-Remaining, X-RateLimit-Used, X-RateLimit-Resource, X-RateLimit-Reset, X-OAuth-Scopes, X-Accepted-OAuth-Scopes, X-Poll-Interval, X-GitHub-Media-Type, X-GitHub-SSO, X-GitHub-Request-Id, Deprecation, Sunset"
      
      $headers$`access-control-allow-origin`
      [1] "*"
      
      $headers$`strict-transport-security`
      [1] "max-age=31536000; includeSubdomains; preload"
      
      $headers$`x-frame-options`
      [1] "deny"
      
      $headers$`x-content-type-options`
      [1] "nosniff"
      
      $headers$`x-xss-protection`
      [1] "0"
      
      $headers$`referrer-policy`
      [1] "origin-when-cross-origin, strict-origin-when-cross-origin"
      
      $headers$`content-security-policy`
      [1] "default-src 'none'"
      
      $headers$vary
      [1] "Accept-Encoding, Accept, X-Requested-With"
      
      $headers$`content-encoding`
      [1] "gzip"
      
      $headers$`x-github-request-id`
      [1] "F98C:2CED39:16FCAA5:17100BF:66155D97"
      
      
      $workflow
      $workflow$id
      [1] 57922738
      
      $workflow$node_id
      [1] "W_kwDOCDHXuc4Dc9Sy"
      
      $workflow$name
      [1] "R-hub"
      
      $workflow$path
      [1] ".github/workflows/rhub.yaml"
      
      $workflow$state
      [1] "active"
      
      $workflow$created_at
      [1] "2023-05-22T13:01:04.000+02:00"
      
      $workflow$updated_at
      [1] "2023-05-22T13:01:04.000+02:00"
      
      $workflow$url
      [1] "https://api.github.com/repos/r-lib/ps/actions/workflows/57922738"
      
      $workflow$html_url
      [1] "https://github.com/r-lib/ps/blob/main/.github/workflows/rhub.yaml"
      
      $workflow$badge_url
      [1] "https://github.com/r-lib/ps/workflows/R-hub/badge.svg"
      
      
      $errors
      NULL
      

