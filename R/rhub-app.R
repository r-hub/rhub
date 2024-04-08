# nocov start

rhub_app <- function() {
  app <- webfakes::new_app()
  app$use("json body parser" = webfakes::mw_json())

  app
}

# nocov end
