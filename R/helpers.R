# dirPackageZips ---------------------------------------------------------------
dirPackageZips <- function(package, path) 
{
  dir(path, paste0("^", package, "_"), full.names = TRUE)
}

# getUrl -----------------------------------------------------------------------
#' @noMd
#' @noRd
#' @keywords internal
getUrl <- function(key, ...)
{
  urls <- resolve(..., x = list(
    cran = "https://cran.r-project.org",
    cran_rstudio = "https://cran.rstudio.org",
    mran_snapshot = "https://mran.microsoft.com/snapshot/<date>",
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
  
  selectElements(urls, key)
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
packageInDestdir <- function(package, destdir, verbose = TRUE)
{
  files <- dirPackageZips(package, safePath(destdir))
  
  file_exists <- length(files) > 0L
  
  if (verbose && file_exists) {
    message("Skipping already downloaded package '", package, "'")
  }
  
  structure(file_exists, path = if (file_exists) lastElement(files))
}


# readDescription --------------------------------------------------------------
readDescription <- function(package)
{
  stopIfNotInstalled(package)
  
  description <- system.file("DESCRIPTION", package = package) %>% 
    kwb.utils::safePath() %>% 
    read.dcf()
  
  colnames(description) <- tolower(colnames(description))
  description
}

# stopIfNotInstalled -----------------------------------------------------------

#' Is a Package Installed?
#' 
#' @param package package name (character vector of length one)
#' @export
stopIfNotInstalled <- function(package)
{
  stopifnot(is.character(package), length(package) == 1L)
  
  available <- rownames(installed.packages())
                        
  if (!package %in% available) {
    kwb.utils::stopFormatted("The package '%s' is not installed.", package)
  }
}
