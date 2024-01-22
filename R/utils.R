# dirPackageZips ---------------------------------------------------------------
dirPackageZips <- function(package, path) 
{
  dir(path, paste0("^", package, "_"), full.names = TRUE)
}

# getUrl -----------------------------------------------------------------------
#' @noMd
#' @noRd
#' @keywords internal
#' @importFrom kwb.utils selectElements
getUrl <- function(key, ...)
{
  urls <- kwb.utils::resolve(..., x = list(
    cran = "https://cran.r-project.org",
    cran_contrib = "<cran>/src/contrib",
    cran_package = "<cran>/web/packages/<package>",
    cran_archive = "<cran_contrib>/Archive/<package>",
    cran_archive_file = "<cran_archive>/<package_filename>",
    cran_package_file = "<cran_contrib>/<package_filename>",
    package_filename = "<package>_<version>.tar.gz",
    github_raw = "https://raw.githubusercontent.com/<repo>",
    github_api = "https://api.github.com/repos/<repo>",
    github_desc = "<github_raw>/<sha>/DESCRIPTION",
    github_releases = "<github_api>/releases",
    github_tags = "<github_api>/tags",
    cached_desc = "DESCRIPTION_<package>_<version>.txt"
  ))
  
  kwb.utils::selectElements(urls, key)
}

# githubRepo -------------------------------------------------------------------
githubRepo <- function(github_user, name)
{
  paste(github_user, name, sep = "/")
}

# packageInDestdir -------------------------------------------------------------
#' @noMd
#' @noRd
#' @keywords internal
#' @importFrom kwb.utils safePath
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

