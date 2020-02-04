library("vcr")
invisible(vcr::vcr_configure(
  dir = "../fixtures",
  filter_sensitive_data = list("<<<my_rhub_token>>>" = rhub:::email_get_token(rhub:::list_validated_emails()[1]),
                               "<<<my_email>>>" = rhub:::list_validated_emails()[[1]])
))
vcr::check_cassette_names()
