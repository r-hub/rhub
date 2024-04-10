# parse_gh_url

    Code
      parse_gh_url("https://github.com/r-lib/cli")
    Output
      $host
      [1] "github.com"
      
      $api
      [1] "https://api.github.com"
      
      $graphql
      [1] "https://api.github.com/graphql"
      
      $user
      [1] "r-lib"
      
      $repo
      [1] "cli"
      
      $slug
      [1] "r-lib/cli"
      
      $pat_url
      [1] "https://github.com/r-lib/cli"
      
    Code
      parse_gh_url("http://github.com/r-lib/cli")
    Output
      $host
      [1] "github.com"
      
      $api
      [1] "http://api.github.com"
      
      $graphql
      [1] "http://api.github.com/graphql"
      
      $user
      [1] "r-lib"
      
      $repo
      [1] "cli"
      
      $slug
      [1] "r-lib/cli"
      
      $pat_url
      [1] "http://github.com/r-lib/cli"
      
    Code
      parse_gh_url("https://my.private.gh/user/repo")
    Output
      $host
      [1] "my.private.gh"
      
      $api
      [1] "https://my.private.gh/api/v3"
      
      $graphql
      [1] "https://my.private.gh/api/graphql"
      
      $user
      [1] "user"
      
      $repo
      [1] "repo"
      
      $slug
      [1] "user/repo"
      
      $pat_url
      [1] "https://my.private.gh/user/repo"
      
    Code
      parse_gh_url("http://my.private.gh/user/repo")
    Output
      $host
      [1] "my.private.gh"
      
      $api
      [1] "http://my.private.gh/api/v3"
      
      $graphql
      [1] "http://my.private.gh/api/graphql"
      
      $user
      [1] "user"
      
      $repo
      [1] "repo"
      
      $slug
      [1] "user/repo"
      
      $pat_url
      [1] "http://my.private.gh/user/repo"
      

# gh_headers

    Code
      gh_headers("mytoken")
    Output
                             Accept                 Authorization 
      "application/vnd.github+json"              "Bearer mytoken" 

# gh_query_process_response

    Code
      gh_query_process_response(resp)
    Output
      $url
      [1] "https://api.github.com/repos/r-hub/rhub/actions/workflows"
      
      $status_code
      [1] 200
      
      $type
      [1] "application/json; charset=utf-8"
      
      $headers
      $headers$server
      [1] "GitHub.com"
      
      $headers$date
      [1] "Tue, 09 Apr 2024 11:54:50 GMT"
      
      $headers$`content-type`
      [1] "application/json; charset=utf-8"
      
      $headers$`cache-control`
      [1] "public, max-age=60, s-maxage=60"
      
      $headers$vary
      [1] "Accept, Accept-Encoding, Accept, X-Requested-With"
      
      $headers$etag
      [1] "W/\"1d4178504dffed82cfb18f9c41c9d471a51bacdec453d315cb4d5fdd76b0ccf9\""
      
      $headers$`x-github-media-type`
      [1] "github.v3; format=json"
      
      $headers$`x-github-api-version-selected`
      [1] "2022-11-28"
      
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
      
      $headers$`content-encoding`
      [1] "gzip"
      
      $headers$`x-ratelimit-limit`
      [1] "60"
      
      $headers$`x-ratelimit-remaining`
      [1] "55"
      
      $headers$`x-ratelimit-reset`
      [1] "1712666671"
      
      $headers$`x-ratelimit-resource`
      [1] "core"
      
      $headers$`x-ratelimit-used`
      [1] "5"
      
      $headers$`accept-ranges`
      [1] "bytes"
      
      $headers$`content-length`
      [1] "455"
      
      $headers$`x-github-request-id`
      [1] "F003:3D4DBC:AF8CD6D:B04554D:66152C8A"
      
      
      $modified
      [1] NA
      
      $times
           redirect    namelookup       connect   pretransfer starttransfer 
           0.000000      0.020028      0.056345      0.099050      0.302180 
              total 
           0.302317 
      
      $content
      $content$total_count
      [1] 5
      
      $content$workflows
      $content$workflows[[1]]
      $content$workflows[[1]]$id
      [1] 33705939
      
      $content$workflows[[1]]$node_id
      [1] "W_kwDOBAK7O84CAk_T"
      
      $content$workflows[[1]]$name
      [1] "R-CMD-check"
      
      $content$workflows[[1]]$path
      [1] ".github/workflows/R-CMD-check.yaml"
      
      $content$workflows[[1]]$state
      [1] "active"
      
      $content$workflows[[1]]$created_at
      [1] "2022-08-31T10:20:12.000Z"
      
      $content$workflows[[1]]$updated_at
      [1] "2022-08-31T10:20:12.000Z"
      
      $content$workflows[[1]]$url
      [1] "https://api.github.com/repos/r-hub/rhub/actions/workflows/33705939"
      
      $content$workflows[[1]]$html_url
      [1] "https://github.com/r-hub/rhub/blob/main/.github/workflows/R-CMD-check.yaml"
      
      $content$workflows[[1]]$badge_url
      [1] "https://github.com/r-hub/rhub/workflows/R-CMD-check/badge.svg"
      
      
      $content$workflows[[2]]
      $content$workflows[[2]]$id
      [1] 33705940
      
      $content$workflows[[2]]$node_id
      [1] "W_kwDOBAK7O84CAk_U"
      
      $content$workflows[[2]]$name
      [1] "pkgdown"
      
      $content$workflows[[2]]$path
      [1] ".github/workflows/pkgdown.yaml"
      
      $content$workflows[[2]]$state
      [1] "active"
      
      $content$workflows[[2]]$created_at
      [1] "2022-08-31T10:20:12.000Z"
      
      $content$workflows[[2]]$updated_at
      [1] "2022-08-31T10:20:12.000Z"
      
      $content$workflows[[2]]$url
      [1] "https://api.github.com/repos/r-hub/rhub/actions/workflows/33705940"
      
      $content$workflows[[2]]$html_url
      [1] "https://github.com/r-hub/rhub/blob/main/.github/workflows/pkgdown.yaml"
      
      $content$workflows[[2]]$badge_url
      [1] "https://github.com/r-hub/rhub/workflows/pkgdown/badge.svg"
      
      
      $content$workflows[[3]]
      $content$workflows[[3]]$id
      [1] 33705941
      
      $content$workflows[[3]]$node_id
      [1] "W_kwDOBAK7O84CAk_V"
      
      $content$workflows[[3]]$name
      [1] "Commands"
      
      $content$workflows[[3]]$path
      [1] ".github/workflows/pr-commands.yaml"
      
      $content$workflows[[3]]$state
      [1] "active"
      
      $content$workflows[[3]]$created_at
      [1] "2022-08-31T10:20:12.000Z"
      
      $content$workflows[[3]]$updated_at
      [1] "2022-08-31T10:20:12.000Z"
      
      $content$workflows[[3]]$url
      [1] "https://api.github.com/repos/r-hub/rhub/actions/workflows/33705941"
      
      $content$workflows[[3]]$html_url
      [1] "https://github.com/r-hub/rhub/blob/main/.github/workflows/pr-commands.yaml"
      
      $content$workflows[[3]]$badge_url
      [1] "https://github.com/r-hub/rhub/workflows/Commands/badge.svg"
      
      
      $content$workflows[[4]]
      $content$workflows[[4]]$id
      [1] 33705942
      
      $content$workflows[[4]]$node_id
      [1] "W_kwDOBAK7O84CAk_W"
      
      $content$workflows[[4]]$name
      [1] "test-coverage"
      
      $content$workflows[[4]]$path
      [1] ".github/workflows/test-coverage.yaml"
      
      $content$workflows[[4]]$state
      [1] "active"
      
      $content$workflows[[4]]$created_at
      [1] "2022-08-31T10:20:12.000Z"
      
      $content$workflows[[4]]$updated_at
      [1] "2022-08-31T10:20:12.000Z"
      
      $content$workflows[[4]]$url
      [1] "https://api.github.com/repos/r-hub/rhub/actions/workflows/33705942"
      
      $content$workflows[[4]]$html_url
      [1] "https://github.com/r-hub/rhub/blob/main/.github/workflows/test-coverage.yaml"
      
      $content$workflows[[4]]$badge_url
      [1] "https://github.com/r-hub/rhub/workflows/test-coverage/badge.svg"
      
      
      $content$workflows[[5]]
      $content$workflows[[5]]$id
      [1] 33708295
      
      $content$workflows[[5]]$node_id
      [1] "W_kwDOBAK7O84CAlkH"
      
      $content$workflows[[5]]$name
      [1] "pages-build-deployment"
      
      $content$workflows[[5]]$path
      [1] "dynamic/pages/pages-build-deployment"
      
      $content$workflows[[5]]$state
      [1] "active"
      
      $content$workflows[[5]]$created_at
      [1] "2022-08-31T10:58:47.000Z"
      
      $content$workflows[[5]]$updated_at
      [1] "2022-08-31T10:58:47.000Z"
      
      $content$workflows[[5]]$url
      [1] "https://api.github.com/repos/r-hub/rhub/actions/workflows/33708295"
      
      $content$workflows[[5]]$html_url
      [1] "https://github.com/r-hub/rhub/actions/workflows/pages/pages-build-deployment"
      
      $content$workflows[[5]]$badge_url
      [1] "https://github.com/r-hub/rhub/actions/workflows/pages/pages-build-deployment/badge.svg"
      
      
      
      

# gh_rest_get, async_gh_rest_get

    Code
      json
    Output
      $url
      [1] "https://api.github.com/repos/r-hub/rhub/actions/workflows"
      
      $status_code
      [1] 200
      
      $type
      [1] "application/json; charset=utf-8"
      
      $headers
      $headers$server
      [1] "GitHub.com"
      
      $headers$date
      [1] "Tue, 09 Apr 2024 11:54:50 GMT"
      
      $headers$`content-type`
      [1] "application/json; charset=utf-8"
      
      $headers$`cache-control`
      [1] "public, max-age=60, s-maxage=60"
      
      $headers$vary
      [1] "Accept, Accept-Encoding, Accept, X-Requested-With"
      
      $headers$etag
      [1] "W/\"1d4178504dffed82cfb18f9c41c9d471a51bacdec453d315cb4d5fdd76b0ccf9\""
      
      $headers$`x-github-media-type`
      [1] "github.v3; format=json"
      
      $headers$`x-github-api-version-selected`
      [1] "2022-11-28"
      
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
      
      $headers$`content-encoding`
      [1] "gzip"
      
      $headers$`x-ratelimit-limit`
      [1] "60"
      
      $headers$`x-ratelimit-remaining`
      [1] "55"
      
      $headers$`x-ratelimit-reset`
      [1] "1712666671"
      
      $headers$`x-ratelimit-resource`
      [1] "core"
      
      $headers$`x-ratelimit-used`
      [1] "5"
      
      $headers$`accept-ranges`
      [1] "bytes"
      
      $headers$`content-length`
      [1] "455"
      
      $headers$`x-github-request-id`
      [1] "F003:3D4DBC:AF8CD6D:B04554D:66152C8A"
      
      
      $modified
      [1] NA
      
      $times
           redirect    namelookup       connect   pretransfer starttransfer 
           0.000000      0.020028      0.056345      0.099050      0.302180 
              total 
           0.302317 
      
      $content
      $content$total_count
      [1] 5
      
      $content$workflows
      $content$workflows[[1]]
      $content$workflows[[1]]$id
      [1] 33705939
      
      $content$workflows[[1]]$node_id
      [1] "W_kwDOBAK7O84CAk_T"
      
      $content$workflows[[1]]$name
      [1] "R-CMD-check"
      
      $content$workflows[[1]]$path
      [1] ".github/workflows/R-CMD-check.yaml"
      
      $content$workflows[[1]]$state
      [1] "active"
      
      $content$workflows[[1]]$created_at
      [1] "2022-08-31T10:20:12.000Z"
      
      $content$workflows[[1]]$updated_at
      [1] "2022-08-31T10:20:12.000Z"
      
      $content$workflows[[1]]$url
      [1] "https://api.github.com/repos/r-hub/rhub/actions/workflows/33705939"
      
      $content$workflows[[1]]$html_url
      [1] "https://github.com/r-hub/rhub/blob/main/.github/workflows/R-CMD-check.yaml"
      
      $content$workflows[[1]]$badge_url
      [1] "https://github.com/r-hub/rhub/workflows/R-CMD-check/badge.svg"
      
      
      $content$workflows[[2]]
      $content$workflows[[2]]$id
      [1] 33705940
      
      $content$workflows[[2]]$node_id
      [1] "W_kwDOBAK7O84CAk_U"
      
      $content$workflows[[2]]$name
      [1] "pkgdown"
      
      $content$workflows[[2]]$path
      [1] ".github/workflows/pkgdown.yaml"
      
      $content$workflows[[2]]$state
      [1] "active"
      
      $content$workflows[[2]]$created_at
      [1] "2022-08-31T10:20:12.000Z"
      
      $content$workflows[[2]]$updated_at
      [1] "2022-08-31T10:20:12.000Z"
      
      $content$workflows[[2]]$url
      [1] "https://api.github.com/repos/r-hub/rhub/actions/workflows/33705940"
      
      $content$workflows[[2]]$html_url
      [1] "https://github.com/r-hub/rhub/blob/main/.github/workflows/pkgdown.yaml"
      
      $content$workflows[[2]]$badge_url
      [1] "https://github.com/r-hub/rhub/workflows/pkgdown/badge.svg"
      
      
      $content$workflows[[3]]
      $content$workflows[[3]]$id
      [1] 33705941
      
      $content$workflows[[3]]$node_id
      [1] "W_kwDOBAK7O84CAk_V"
      
      $content$workflows[[3]]$name
      [1] "Commands"
      
      $content$workflows[[3]]$path
      [1] ".github/workflows/pr-commands.yaml"
      
      $content$workflows[[3]]$state
      [1] "active"
      
      $content$workflows[[3]]$created_at
      [1] "2022-08-31T10:20:12.000Z"
      
      $content$workflows[[3]]$updated_at
      [1] "2022-08-31T10:20:12.000Z"
      
      $content$workflows[[3]]$url
      [1] "https://api.github.com/repos/r-hub/rhub/actions/workflows/33705941"
      
      $content$workflows[[3]]$html_url
      [1] "https://github.com/r-hub/rhub/blob/main/.github/workflows/pr-commands.yaml"
      
      $content$workflows[[3]]$badge_url
      [1] "https://github.com/r-hub/rhub/workflows/Commands/badge.svg"
      
      
      $content$workflows[[4]]
      $content$workflows[[4]]$id
      [1] 33705942
      
      $content$workflows[[4]]$node_id
      [1] "W_kwDOBAK7O84CAk_W"
      
      $content$workflows[[4]]$name
      [1] "test-coverage"
      
      $content$workflows[[4]]$path
      [1] ".github/workflows/test-coverage.yaml"
      
      $content$workflows[[4]]$state
      [1] "active"
      
      $content$workflows[[4]]$created_at
      [1] "2022-08-31T10:20:12.000Z"
      
      $content$workflows[[4]]$updated_at
      [1] "2022-08-31T10:20:12.000Z"
      
      $content$workflows[[4]]$url
      [1] "https://api.github.com/repos/r-hub/rhub/actions/workflows/33705942"
      
      $content$workflows[[4]]$html_url
      [1] "https://github.com/r-hub/rhub/blob/main/.github/workflows/test-coverage.yaml"
      
      $content$workflows[[4]]$badge_url
      [1] "https://github.com/r-hub/rhub/workflows/test-coverage/badge.svg"
      
      
      $content$workflows[[5]]
      $content$workflows[[5]]$id
      [1] 33708295
      
      $content$workflows[[5]]$node_id
      [1] "W_kwDOBAK7O84CAlkH"
      
      $content$workflows[[5]]$name
      [1] "pages-build-deployment"
      
      $content$workflows[[5]]$path
      [1] "dynamic/pages/pages-build-deployment"
      
      $content$workflows[[5]]$state
      [1] "active"
      
      $content$workflows[[5]]$created_at
      [1] "2022-08-31T10:58:47.000Z"
      
      $content$workflows[[5]]$updated_at
      [1] "2022-08-31T10:58:47.000Z"
      
      $content$workflows[[5]]$url
      [1] "https://api.github.com/repos/r-hub/rhub/actions/workflows/33708295"
      
      $content$workflows[[5]]$html_url
      [1] "https://github.com/r-hub/rhub/actions/workflows/pages/pages-build-deployment"
      
      $content$workflows[[5]]$badge_url
      [1] "https://github.com/r-hub/rhub/actions/workflows/pages/pages-build-deployment/badge.svg"
      
      
      
      

# gh_rest_post, async_gh_rest_post

    Code
      json
    Output
      $url
      [1] "https://api.github.com/repos/r-lib/ps/actions/workflows/rhub.yaml/dispatches"
      
      $status_code
      [1] 204
      
      $type
      [1] NA
      
      $headers
      $headers$server
      [1] "GitHub.com"
      
      $headers$date
      [1] "Tue, 09 Apr 2024 12:09:42 GMT"
      
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
      [1] "4979"
      
      $headers$`x-ratelimit-reset`
      [1] "1712665652"
      
      $headers$`x-ratelimit-used`
      [1] "21"
      
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
      
      $headers$`x-github-request-id`
      [1] "F0EC:3056D9:20B42811:20D7C1CF:66153006"
      
      
      $modified
      [1] NA
      
      $times
           redirect    namelookup       connect   pretransfer starttransfer 
           0.000000      0.013760      0.048724      0.086173      0.337608 
              total 
           0.341246 
      
      $content
      raw(0)
      

# gh_gql_get, async_gh_gql_get

    Code
      json
    Output
      $url
      [1] "https://api.github.com/repos/r-lib/ps/actions/workflows/rhub.yaml"
      
      $status_code
      [1] 200
      
      $type
      [1] "application/json; charset=utf-8"
      
      $headers
      $headers$server
      [1] "GitHub.com"
      
      $headers$date
      [1] "Tue, 09 Apr 2024 13:01:07 GMT"
      
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
      [1] "4982"
      
      $headers$`x-ratelimit-reset`
      [1] "1712669308"
      
      $headers$`x-ratelimit-used`
      [1] "18"
      
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
      [1] "F324:3E972F:3CB25779:3CEEE5ED:66153C13"
      
      
      $modified
      [1] NA
      
      $times
           redirect    namelookup       connect   pretransfer starttransfer 
           0.000000      0.045222      0.085429      0.123010      0.299343 
              total 
           0.303010 
      
      $content
      $content$id
      [1] 57922738
      
      $content$node_id
      [1] "W_kwDOCDHXuc4Dc9Sy"
      
      $content$name
      [1] "R-hub"
      
      $content$path
      [1] ".github/workflows/rhub.yaml"
      
      $content$state
      [1] "active"
      
      $content$created_at
      [1] "2023-05-22T13:01:04.000+02:00"
      
      $content$updated_at
      [1] "2023-05-22T13:01:04.000+02:00"
      
      $content$url
      [1] "https://api.github.com/repos/r-lib/ps/actions/workflows/57922738"
      
      $content$html_url
      [1] "https://github.com/r-lib/ps/blob/main/.github/workflows/rhub.yaml"
      
      $content$badge_url
      [1] "https://github.com/r-lib/ps/workflows/R-hub/badge.svg"
      
      

