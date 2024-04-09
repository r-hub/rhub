test_that("rhub_check", {
  withr::local_envvar(
    RHUB_PLATFORMS_URL = http$url("/platforms.json"),
    RHUB_CONTAINERS_URL = http$url("/manifest.json")
  )

  mockery::stub(rhub_check, "doctor_find_pat", "secret")

  presp <- gh_query_process_response(
    readRDS(test_path("fixtures/gh-response-post.rds"))
  )
  mockery::stub(rhub_check, "gh_rest_post", presp)
  mockery::stub(rhub_check, "random_id", "kleptomaniac-harlequinbug")
  expect_snapshot({
    rhub_check(
      "https://github.com/r-lib/ps",
      platforms = c("linux", "clang18")
    )
  })

  # error
  presp$status_code <- 401L
  presp$content <- list(message = "I am so, so sorry!")
  mockery::stub(rhub_check, "gh_rest_post", presp)
  expect_snapshot(error = TRUE, {
    rhub_check(
      "https://github.com/r-lib/ps",
      platforms = c("linux", "clang18")
    )
  })

  # looks up current branch if it is needed
  presp$status_code <- 204L
  mockery::stub(rhub_check, "gh_rest_post", presp)
  mockery::stub(rhub_check, "setup_find_git_root", getwd())
  mockery::stub(rhub_check, "doctor_find_gh_url", "https://github.com/r-lib/ps")
  mockery::stub(rhub_check, "gert::git_branch", "main")
  expect_snapshot({
    rhub_check(platforms = c("linux", "clang18"))
  })
})
