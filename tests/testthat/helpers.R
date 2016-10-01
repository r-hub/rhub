
create_minimal_package <- function(dir = tempfile()) {
  if (!file.exists(dir)) dir.create(dir)

  ## /R This is not strictly necessary, actually
  dir.create(file.path(dir, "R"))
  cat("f <- function() { }\n", file = file.path(dir, "R", "package.R"))

  ## NAMESPACE
  cat("", file = file.path(dir, "NAMESPACE"))

  ## DESCRIPTION
  desc::description$new("!new")$
    set(Package = basename(dir))$
    set(Title = "Title Case")$
    set(Maintainer = "first second <first.second@foo.bar>")$
    set(Description = "Minimal package for testing. Multiple.")$
    set(License = "GPL-2")$
    del("URL")$
    del("BugReports")$
    write(file.path(dir, "DESCRIPTION"))

  dir
}
