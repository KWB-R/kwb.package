# loadDescriptionFromArchiveUrl ------------------------------------------------
loadDescriptionFromArchiveUrl <- function(url)
{
  stopifnot(endsWith(url, ".tar.gz"))
  
  destfile <- tempfile()
  
  utils::download.file(url, destfile)
  on.exit(unlink(destfile))
  
  remotes:::load_pkg_description(destfile)
}
