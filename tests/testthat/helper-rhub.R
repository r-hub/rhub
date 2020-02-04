# comment the commands below before recording test cassettes
mockery::stub(where = list_validated_emails2,
              what = "email_file", 
              how = file.path(testthat::test_path(), "validated_emails.csv"))
mockery::stub(where = email_get_token,
              what = "email_file", 
              how = file.path(testthat::test_path(), "validated_emails.csv"))
mockery::stub(where = email_add_token,
              what = "email_file", 
              how = file.path(testthat::test_path(), "validated_emails.csv"))

library("vcr")
invisible(vcr::vcr_configure(
  dir = "../fixtures",
  filter_sensitive_data = list("<<<my_rhub_token>>>" = email_get_token(list_validated_emails()[[1]]),
                               "<<<my_email>>>" = list_validated_emails()[[1]])
))


