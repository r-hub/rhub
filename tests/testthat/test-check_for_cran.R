vcr::use_cassette("check_for_cran", {
  test_that("check_for_cran works", {
    
    pkg <- create_minimal_package(packagename = "myminimalpackage")

    pkg_targz <- build_package(pkg, tempfile())

    crancheck <- check_for_cran(pkg_targz,
                                email = list_validated_emails()[[1]])

    expect_is(crancheck, "rhub_check")
    expect_is(crancheck$urls(), "data.frame")
    
    recreate_email()
  })
})