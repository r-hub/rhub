# rc_new_token

    Code
      rc_new_token()
    Condition
      Error:
      ! No email or no token and not in interactive mode
    Code
      rc_new_token(email = "user@example.com")
    Condition
      Error:
      ! No email or no token and not in interactive mode
    Code
      rc_new_token(token = "secret")
    Condition
      Error:
      ! No email or no token and not in interactive mode

---

    Code
      rc_new_token("user@example.com", "secret")
    Message
      v Added token for "user@example.com".
      i R-hub tokens are stored at '<email-file>'.

# rc_list_repos

    Code
      rc_list_repos(email = "csardi.gabor@gmail.com")
    Output
                              repo_name
      1  uncrystallised-groundhog-callr
      2 uncrystallised-groundhog-dotenv
      3   uncrystallised-groundhog-rhub
      4   uncrystallised-groundhog-tiff
                                                         repo_url
      1  https://github.com/r-hub2/uncrystallised-groundhog-callr
      2 https://github.com/r-hub2/uncrystallised-groundhog-dotenv
      3   https://github.com/r-hub2/uncrystallised-groundhog-rhub
      4   https://github.com/r-hub2/uncrystallised-groundhog-tiff
                                                               builds_url
      1  https://github.com/r-hub2/uncrystallised-groundhog-callr/actions
      2 https://github.com/r-hub2/uncrystallised-groundhog-dotenv/actions
      3   https://github.com/r-hub2/uncrystallised-groundhog-rhub/actions
      4   https://github.com/r-hub2/uncrystallised-groundhog-tiff/actions

# rc_submit

    Code
      rc_submit()
    Condition
      Error:
      ! You need to set `confirmation` to "TRUE" to submit packages to R-hub from non-interactive R sessions.

---

    Code
      rc_submit(pkg, confirmation = TRUE)
      (rc_submit(pkg, confirmation = TRUE))
    Output
      $result
      [1] "OK"
      
      $repo_url
      [1] "https://github.com/r-hub2/uncrystallised-groundhog-rhub"
      
      $actions_url
      [1] "https://github.com/r-hub2/uncrystallised-groundhog-rhub/actions"
      
      $id
      [1] "eonian-crustacean"
      
      $name
      [1] "linux,clang18"
      

---

    Code
      (rc_submit(pkg))
    Output
      -- Confirmation ----------------------------------------------------------------
    Message
      ! Your package will be publicly readable at <https://github.com/r-hub2>.
      > You will need a GitHub account to view the build logs.
      > Only continue if you are fine with this.
      > See the `rhub_setup()` function for an alternative way of using R-hub.
    Output
      
      Please type 'yes' to continue: no
    Message
      
    Condition
      Error:
      ! Aborted R-hub submission.

---

    Code
      (rc_submit(pkg))
    Output
      -- Confirmation ----------------------------------------------------------------
    Message
      ! Your package will be publicly readable at <https://github.com/r-hub2>.
      > You will need a GitHub account to view the build logs.
      > Only continue if you are fine with this.
      > See the `rhub_setup()` function for an alternative way of using R-hub.
    Output
      
      Please type 'yes' to continue: yes
    Message
      
    Output
      $result
      [1] "OK"
      
      $repo_url
      [1] "https://github.com/r-hub2/uncrystallised-groundhog-rhub"
      
      $actions_url
      [1] "https://github.com/r-hub2/uncrystallised-groundhog-rhub/actions"
      
      $id
      [1] "eonian-crustacean"
      
      $name
      [1] "linux,clang18"
      

---

    Code
      rc_submit(pkg)
    Condition
      Error:
      ! Could not query R package name at 'fixtures/bad.tar.gz'.
      i Make sure that `path` is an R package or a directory containing an R package.

---

    Code
      (rc_submit(pkg))
    Output
      -- R CMD build -----------------------------------------------------------------
      * checking for file '<path> ... OK
      * preparing 'pkg':
      * checking DESCRIPTION meta-information ... OK
      * checking for LF line-endings in source and make files and shell scripts
      * checking for empty or unneeded directories
      * building 'pkg_0.0.0.9000.tar.gz'
      
      -- Confirmation ----------------------------------------------------------------
    Message
      ! Your package will be publicly readable at <https://github.com/r-hub2>.
      > You will need a GitHub account to view the build logs.
      > Only continue if you are fine with this.
      > See the `rhub_setup()` function for an alternative way of using R-hub.
    Output
      
      Please type 'yes' to continue: yes
    Message
      
    Output
      $result
      [1] "OK"
      
      $repo_url
      [1] "https://github.com/r-hub2/uncrystallised-groundhog-rhub"
      
      $actions_url
      [1] "https://github.com/r-hub2/uncrystallised-groundhog-rhub/actions"
      
      $id
      [1] "eonian-crustacean"
      
      $name
      [1] "linux,clang18"
      

---

    Code
      (rc_submit(pkg))
    Output
      -- Confirmation ----------------------------------------------------------------
    Message
      ! Your package will be publicly readable at <https://github.com/r-hub2>.
      > You will need a GitHub account to view the build logs.
      > Only continue if you are fine with this.
      > See the `rhub_setup()` function for an alternative way of using R-hub.
    Output
      
      Please type 'yes' to continue: yes
    Message
      
    Condition
      Error:
      ! Invalid response from R-hub server, please report this.

# guess_email

    Code
      guess_email()
    Message
      i Using maintainer email address "user@example.com".
    Output
      [1] "user@example.com"
    Code
      guess_email(message = FALSE)
    Output
      [1] "user@example.com"

---

    Code
      guess_email()
    Message
      i Using email address "another@example.com".
    Output
      [1] "another@example.com"

---

    Code
      guess_email(message = FALSE)
    Output
      [1] "another@example.com"

# get_auth_header

    Code
      get_auth_header("csardi.gabor@gmail.com")
    Output
        Authorization 
      "Bearer token1" 

---

    Code
      get_auth_header("user@example.com")
    Condition
      Error:
      ! Can't find token for email address "user@example.com".
      i Call `rhub::rc_new_token()` to get a token.

# get_email_to_validate

    Code
      get_email_to_validate(".")
    Output
      
      -- Choose email address to request token for (or 0 to exit)
      1: v  csardi.gabor@gmail.com
      2: v  csardi.gabor+new@gmail.com
      3: v  csardi.gabor+another@gmail.com
      4: v  csardi.gabor+fake@gmail.com
      5:    user@example.com
      6:    maint@example.com
      7:    New email address
      
      Selection: 0
    Condition
      Error:
      ! Cancelled requesting new token

---

    Code
      get_email_to_validate(".")
    Output
      
      -- Choose email address to request token for (or 0 to exit)
      1: v  csardi.gabor@gmail.com
      2: v  csardi.gabor+new@gmail.com
      3: v  csardi.gabor+another@gmail.com
      4: v  csardi.gabor+fake@gmail.com
      5:    user@example.com
      6:    maint@example.com
      7:    New email address
      
      Selection: 5
      [1] "user@example.com"

---

    Code
      get_email_to_validate(".")
    Output
      
      -- Choose email address to request token for (or 0 to exit)
      1: v  csardi.gabor@gmail.com
      2: v  csardi.gabor+new@gmail.com
      3: v  csardi.gabor+another@gmail.com
      4: v  csardi.gabor+fake@gmail.com
      5:    user@example.com
      6:    maint@example.com
      7:    New email address
      
      Selection: 7
      Email address: custom@example.com
      [1] "custom@example.com"

---

    Code
      get_email_to_validate(".")
    Output
      
      Email address: custom@example.com
      [1] "custom@example.com"

# list_validated_emails2

    Code
      list_validated_emails2(message = FALSE)
    Output
                                 email  token
      1         csardi.gabor@gmail.com token1
      2     csardi.gabor+new@gmail.com token2
      3 csardi.gabor+another@gmail.com token3
      4    csardi.gabor+fake@gmail.com token4
    Code
      list_validated_emails2(message = TRUE)
    Message
      > R-hub tokens are stored at 'fixtures/validated_emails.csv'.
    Output
                                 email  token
      1         csardi.gabor@gmail.com token1
      2     csardi.gabor+new@gmail.com token2
      3 csardi.gabor+another@gmail.com token3
      4    csardi.gabor+fake@gmail.com token4

---

    Code
      list_validated_emails2(message = FALSE)
    Message
      i No R-hub tokens found.
    Code
      list_validated_emails2(message = TRUE)
    Message
      i No R-hub tokens found.

# email_file

    Code
      email_file()
    Output
      [1] "/config/validated_emails.csv"

# rc_new_token_interactive

    Code
      rc_new_token_interactive(email = "maint@example.com")
    Message
      i Please check your emails for the R-hub access token.
    Output
      [[1]]
      [1] "maint@example.com"
      
      [[2]]
      [1] "token"
      

---

    Code
      rc_new_token_interactive()
    Message
      i Please check your emails for the R-hub access token.
    Output
      [[1]]
      [1] "user@example.com"
      
      [[2]]
      [1] "token"
      

# email_add_token

    Code
      read_token_file(ef)
    Output
                       email     token
      1 newemail@example.com new-token

---

    Code
      read_token_file(ef)
    Output
                        email      token
      1  newemail@example.com  new-token
      2 newemail2@example.com new-token2

---

    Code
      read_token_file(ef)
    Output
                        email         token
      1  newemail@example.com new-new-token
      2 newemail2@example.com    new-token2

