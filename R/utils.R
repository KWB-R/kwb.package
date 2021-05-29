# getUrl -----------------------------------------------------------------------
getUrl <- function(key, ...)
{
  urls <- kwb.utils::resolve(..., x = list(
    cran = "https://cran.r-project.org",
    cran_contrib = "<cran>/src/contrib",
    cran_package = "<cran>/web/packages/<package>",
    cran_archive = "<cran_contrib>/Archive/<package>",
    cran_archive_file = "<cran_archive>/<package_filename>",
    cran_package_file = "<cran_contrib>/<package_filename>",
    package_filename = "<package>_<version>.tar.gz"
  ))
  
  kwb.utils::selectElements(urls, key)
}

# githubRepo -------------------------------------------------------------------
githubRepo <- function(github_user, name)
{
  paste(github_user, name, sep = "/")
}

# packageInDestdir -------------------------------------------------------------
packageInDestdir <- function(package, destdir, verbose = TRUE)
{
  files <- dirPackageZips(package, kwb.utils::safePath(destdir))
  
  file_exists <- length(files) > 0L
  
  if (verbose && file_exists) {
    message("Skipping already downloaded package '", package, "'")
  }
  
  structure(file_exists, path = if (file_exists) kwb.utils::lastElement(files))
}

# readLinesFromUrl -------------------------------------------------------------
readLinesFromUrl <- function(url, silent = TRUE)
{
  result <- try(suppressWarnings(readLines(url)), silent = silent)
  
  if (inherits(result, "try-error")) {
    return(NULL)
  }
  
  result
}

# stop_ ------------------------------------------------------------------------
stop_ <- function(...)
{
  stop(..., call. = FALSE)
}

# dirPackageZips ---------------------------------------------------------------
dirPackageZips <- function(package, path) 
{
  dir(path, paste0("^", package, "_"), full.names = TRUE)
}
