# parse_gh_url

    Code
      parse_gh_url("https://github.com/foo/bar.git")
    Output
      $host
      [1] "github.com"
      
      $api
      [1] "https://api.github.com"
      
      $graphql
      [1] "https://api.github.com/graphql"
      
      $user
      [1] "foo"
      
      $repo
      [1] "bar"
      
      $slug
      [1] "foo/bar"
      
    Code
      parse_gh_url("https://myserver.org/foo/bar.git")
    Output
      $host
      [1] "myserver.org"
      
      $api
      [1] "https://myserver.org/api/v3"
      
      $graphql
      [1] "https://myserver.org/api/graphql"
      
      $user
      [1] "foo"
      
      $repo
      [1] "bar"
      
      $slug
      [1] "foo/bar"
      

