# pkg_error

    Code
      err
    Output
      <rlib_error_3_1/rlib_error/error>
      Error: 
      ! This is not good!
      i You should not use `foo`, use `bar` instead.

# stop

    Code
      err
    Output
      <rlib_error_3_1/rlib_error/error>
      Error: 
      ! This is not good!
      i You should not use `foo`, use `bar` instead.

# stop with message

    Code
      err
    Output
      <rlib_error_3_1/rlib_error/error>
      Error in `stop("Ooopsie daily!")`:
      ! Ooopsie daily!

# stopifnot

    Code
      stopifnot(1 == 2)
    Condition
      Error:
      ! `1` must equal `2`.

# zip

    Code
      zip(character(), character())
    Output
      list()
    Code
      zip(letters[1:5], LETTERS[1:5])
    Output
      [[1]]
      [1] "a" "A"
      
      [[2]]
      [1] "b" "B"
      
      [[3]]
      [1] "c" "C"
      
      [[4]]
      [1] "d" "D"
      
      [[5]]
      [1] "e" "E"
      
    Code
      zip("1", letters[1:5])
    Output
      [[1]]
      [1] "1" "a"
      
      [[2]]
      [1] "1" "b"
      
      [[3]]
      [1] "1" "c"
      
      [[4]]
      [1] "1" "d"
      
      [[5]]
      [1] "1" "e"
      

# unquote

    Code
      unquote("'quoted'")
    Output
      [1] "quoted"
    Code
      unquote(c("'quoted'", "not", "\"quoted\""))
    Output
      [1] "quoted" "not"    "quoted"

# parse_url

    Code
      parse_url("https://github.com/r-hub/rhub")
    Output
        protocol       host        path
      1    https github.com /r-hub/rhub
    Code
      parse_url("https://user@github.com/r-hub/rhub")
    Output
        protocol       host        path
      1    https github.com /r-hub/rhub
    Code
      parse_url("https://user:pass@github.com/r-hub/rhub")
    Output
        protocol       host        path
      1    https github.com /r-hub/rhub
    Code
      parse_url("https://github.com/r-hub/rhub?q=foo&p=bar")
    Output
        protocol       host                    path
      1    https github.com /r-hub/rhub?q=foo&p=bar
    Code
      parse_url("git@github.com:/r-hub/rhub")
    Output
        protocol       host        path
      1    https github.com /r-hub/rhub
    Code
      parse_url("git@github.com:/r-hub/rhub.git")
    Output
        protocol       host        path
      1    https github.com /r-hub/rhub

---

    Code
      parse_url("this is not a URL at all")
    Condition
      Error in `parse_url()`:
      ! Invalid URL: <this is not a URL at all>

# ansi_align_width [plain]

    Code
      paste0("--", ansi_align_width(c("foo", "bar", "foobar")), "--")
    Output
      [1] "--foo   --" "--bar   --" "--foobar--"
    Code
      paste0("--", ansi_align_width(c("foo", "bar", cli::col_red("foobar"))), "--")
    Output
      [1] "--foo   --" "--bar   --" "--foobar--"
    Code
      ansi_align_width(character())
    Output
      character(0)

# ansi_align_width [ansi]

    Code
      paste0("--", ansi_align_width(c("foo", "bar", "foobar")), "--")
    Output
      [1] "--foo   --" "--bar   --" "--foobar--"
    Code
      paste0("--", ansi_align_width(c("foo", "bar", cli::col_red("foobar"))), "--")
    Output
      [1] "--foo   --"                 "--bar   --"                
      [3] "--\033[31mfoobar\033[39m--"
    Code
      ansi_align_width(character())
    Output
      character(0)

