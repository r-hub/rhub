checkfuns <- new.env()

.onAttach <- function(lib, pkg){
  packageStartupMessage("Loading supported platforms...")
  names <- platforms()$name
  lapply(names, function(platform){
    fun <- paste0("check_", gsub("-", "_", platform))
    assign(fun, function(...){
      check(platform = platform, ...)
    }, checkfuns)
  })
  attach(checkfuns, name = "rhubs")
}

.onDetach <- function(lib, pkg){
  try(detach("rhubs"), silent = TRUE)
}
