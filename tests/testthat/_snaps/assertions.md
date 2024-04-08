# is_character

    Code
      is_character(character())
    Output
      [1] TRUE
    Code
      is_character("a")
    Output
      [1] TRUE
    Code
      is_character(c("a", "b", "c"))
    Output
      [1] TRUE

---

    Code
      x <- 1
      assert_that(is_character(x))
    Condition
      Error:
      ! `x` must be a character vector without `NA`, but it is a number.
    Code
      x <- mtcars
      assert_that(is_character(x))
    Condition
      Error:
      ! `x` must be a character vector without `NA`, but it is a data frame.
    Code
      x <- NULL
      assert_that(is_character(x))
    Condition
      Error:
      ! `x` must be a character vector without `NA`, but it is NULL.
    Code
      x <- c("a", "b", NA_character_)
      assert_that(is_character(x))
    Condition
      Error:
      ! `x` must be a character vector without `NA`, but it has 1 `NA` value.

# is_optional_character

    Code
      is_optional_character(NULL)
    Output
      [1] TRUE
    Code
      is_optional_character(character())
    Output
      [1] TRUE
    Code
      is_optional_character("a")
    Output
      [1] TRUE
    Code
      is_optional_character(c("a", "b", "c"))
    Output
      [1] TRUE

---

    Code
      x <- 1
      assert_that(is_optional_character(x))
    Condition
      Error:
      ! `x` must be a character vector without `NA`, or NULL, but it is a number.
    Code
      x <- mtcars
      assert_that(is_optional_character(x))
    Condition
      Error:
      ! `x` must be a character vector without `NA`, or NULL, but it is a data frame.
    Code
      x <- c("a", "b", NA_character_)
      assert_that(is_optional_character(x))
    Condition
      Error:
      ! `x` must not have `NA` values, but it has 1 `NA` value.

# is_string

    Code
      is_string("a")
    Output
      [1] TRUE

---

    Code
      x <- 1
      assert_that(is_string(x))
    Condition
      Error:
      ! `x` must be a string (character scalar), but it is a number.
    Code
      x <- mtcars
      assert_that(is_string(x))
    Condition
      Error:
      ! `x` must be a string (character scalar), but it is a data frame.
    Code
      x <- NULL
      assert_that(is_string(x))
    Condition
      Error:
      ! `x` must be a string (character scalar), but it is NULL.
    Code
      x <- NA_character_
      assert_that(is_string(x))
    Condition
      Error:
      ! `x` must not be `NA`.
    Code
      x <- c("a", "b", NA_character_)
      assert_that(is_string(x))
    Condition
      Error:
      ! `x` must be a string (character scalar), but it is a character vector.
    Code
      x <- character()
      assert_that(is_string(x))
    Condition
      Error:
      ! `x` must be a string (character scalar), but it is an empty character vector.
    Code
      x <- c("a", "b")
      assert_that(is_string(x))
    Condition
      Error:
      ! `x` must be a string (character scalar), but it is a character vector.

# is_optional_string

    Code
      is_optional_string("a")
    Output
      [1] TRUE
    Code
      is_optional_string(NULL)
    Output
      [1] TRUE

---

    Code
      x <- 1
      assert_that(is_optional_string(x))
    Condition
      Error:
      ! `x` must be a string (character scalar) or NULL, but it is a number.
    Code
      x <- mtcars
      assert_that(is_optional_string(x))
    Condition
      Error:
      ! `x` must be a string (character scalar) or NULL, but it is a data frame.
    Code
      x <- NA_character_
      assert_that(is_optional_string(x))
    Condition
      Error:
      ! `x` must be a string (character scalar) or NULL, but it is a character `NA`.
    Code
      x <- c("a", "b", NA_character_)
      assert_that(is_optional_string(x))
    Condition
      Error:
      ! `x` must be a string (character scalar) or NULL, but it is a character vector.
    Code
      x <- character()
      assert_that(is_optional_string(x))
    Condition
      Error:
      ! `x` must be a string (character scalar) or NULL, but it is an empty character vector.
    Code
      x <- c("a", "b")
      assert_that(is_optional_string(x))
    Condition
      Error:
      ! `x` must be a string (character scalar) or NULL, but it is a character vector.

# is_optional_gh_url

    Code
      is_optional_gh_url(NULL)
    Output
      [1] TRUE
    Code
      is_optional_gh_url("https://github.com")
    Output
      [1] TRUE
    Code
      is_optional_gh_url("http://github.com")
    Output
      [1] TRUE

---

    Code
      gh_url <- 1:10
      assert_that(is_optional_gh_url(gh_url))
    Condition
      Error:
      ! `gh_url` must be a character string. You supplied an integer vector.
    Code
      gh_url <- "foobar"
      assert_that(is_optional_gh_url(gh_url))
    Condition
      Error:
      ! `gh_url` must be an HTTP or HTTPS URL. You supplied: "foobar".

