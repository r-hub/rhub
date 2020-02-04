# to record delete tests/testthat/validated_emails.csv
# and delete the fixtures/ folder

recreate_email <- function() {
  write.table(data.frame(e = "foo@bar.com",
                         t = "lalala"),
              file.path(testthat::test_path(), "validated_emails.csv"),
              sep = ",",
              col.names = FALSE,
              row.names = FALSE)
}

library("vcr")
invisible(vcr::vcr_configure(
  dir = "../fixtures",
  filter_sensitive_data = list("<<<my_rhub_token>>>" = email_get_token(list_validated_emails()[[1]]),
                               "<<<my_email>>>" = list_validated_emails()[[1]])
))


