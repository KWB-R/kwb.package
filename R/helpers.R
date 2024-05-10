# copyBasePackages -------------------------------------------------------------

#' Copy Base R Packages from the System Library to the Target Library

#' @param target_lib path to the target library
#' @param set_number number defining the base packages to be copied, see
#'   \code{\link{systemPackages}}
#' @param system_lib path to the system library from which to copy packages
#' @param packages vector of names of packages to be copied
#' @export
#' @importFrom utils tail
copyBasePackages <- function(
    target_lib, 
    set_number = 2L,
    system_lib = utils::tail(.libPaths(), 1L), 
    packages = systemPackages(set_number))
{
  catAndRun(paste("Copying base R packages to", target_lib), {
    copyFile(
      from = file.path(system_lib, packages), 
      to = target_lib, 
      recursive = TRUE
    )
  })
}

# dirPackageZips ---------------------------------------------------------------
dirPackageZips <- function(package, path) 
{
  dir(path, paste0("^", package, "_"), full.names = TRUE)
}

# getAuthors -------------------------------------------------------------------

#' Get Information on Package Authors
#' 
#' @param package name of (installed) package
#' @export
getAuthors <- function(package)
{
  description <- readDescription(package)
  columns <- colnames(description)
  columns <- grep("author", columns, value = TRUE, ignore.case = TRUE)
  description[, columns]
}

# getPackageLicences -----------------------------------------------------------

#' Which Licences are Specified for the Packages?
#' 
#' @param packages names of (installed) packages
#' @param db optional. Package database, similar to what is returned by
#'   \code{\link[utils]{installed.packages}}. Default:
#'   \code{installed.packages()}
#' @return data frame
#' @importFrom utils installed.packages
#' @export
getPackageLicences <- function(
    packages, 
    db = utils::installed.packages()
)
{
  #kwb.utils::assignPackageObjects("kwb.package");stop.on.error = FALSE
  #`%>%` <- magrittr::`%>%`
  #db <- kwb.utils:::get_cached("package_db")
  #packages <- db$Package
  
  db <- as.data.frame(db)
  names(db) <- gsub("license", "licence", tolower(names(db)))
  
  licence_fields <- grep("^licence", names(db), value = TRUE)
  stopifnot(length(licence_fields) > 0L)
  
  result <- merge(
    x = noFactorDataFrame(package = packages),
    y = selectColumns(db, c("package", "version", licence_fields)), 
    by = "package", 
    all.x = TRUE
  ) %>% 
    renameColumns(list(license = "licence")) %>% 
    orderBy("package")
  
  result[["licence"]] <- defaultIfNa(result[["licence"]], "<not_found>")
  
  result
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
    cached_desc = "DESCRIPTION_<package_and_version>.txt"
  ))
  
  selectElements(urls, key)
}

# getRVersionMajorMinor --------------------------------------------------------

#' Helper: Get R major minor version string 
#'
#' @return returns R version major.minor string (e.g. 4.0), used by standard R 
#' libraries for grouping all R packages into one folder
#' @export
#' @examples
#' getRVersionMajorMinor()
getRVersionMajorMinor <- function()
{
  paste(version$major, strsplit(version$minor, "\\.")[[1L]][1L], sep = ".")
}

# githubRepo -------------------------------------------------------------------
githubRepo <- function(github_user, name)
{
  paste(github_user, name, sep = "/")
}

# hasGplLicence ----------------------------------------------------------------

#' Do Packages have a GPL Licence?
#' 
#' @param packages package name(s) as a vector of character
#' @returns vector of logical 
#' @export
hasGplLicence <- function(packages)
{
  getPackageLicences(packages) %>% 
    selectColumns("licence") %>% 
    grep(pattern = "GPL", ignore.case = TRUE)
}

# installRemotes ---------------------------------------------------------------

#' Install the remotes Package to the Given Library
#' 
#' @param lib path to the library to which to install the remotes package
#' @export
#' @importFrom utils install.packages
#' 
installRemotes <- function(lib)
{
  package_remotes <- "package:remotes"
  
  if (package_remotes %in% search()) {
    try(detach(package_remotes, unload = TRUE))
  }
  
  utils::install.packages("remotes", lib = lib)
}

# isOnCran ---------------------------------------------------------------------
isOnCran <- function(name)
{
  nrow(currentCranVersion(name)) > 0L
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


# packageString ----------------------------------------------------------------

#' Package String
#' 
#' @param package Package name
#' @return \code{package}, preceded by \code{package:}
#' @export
packageString <- function(package)
{
  paste0("package:", package)
}

# readDescription --------------------------------------------------------------
readDescription <- function(package, stop.on.error = TRUE)
{
  if (stop.on.error) {
    stopIfNotInstalled(package)
  }
  
  file <- system.file("DESCRIPTION", package = package)
  
  if (!file.exists(file) ) {
    return(NULL)
  }
  
  result <- read.dcf(file)
  
  colnames(result) <- tolower(colnames(result))
  
  result
}

# readDescriptionAddingPath ----------------------------------------------------
readDescriptionAddingPath <- function(descriptionFile)
{
  # see remotes:::load_pkg_description
  description <- remotes_read_dcf(descriptionFile)
  names(description) <- tolower(names(description))
  description$path <- descriptionFile
  description
}

# stopIfNotInstalled -----------------------------------------------------------

#' Is a Package Installed?
#' 
#' @param package package name (character vector of length one)
#' @importFrom utils installed.packages
#' @export
stopIfNotInstalled <- function(package)
{
  stopifnot(is.character(package), length(package) == 1L)
  
  available <- rownames(utils::installed.packages())
                        
  if (!package %in% available) {
    stopFormatted("The package '%s' is not installed.", package)
  }
}

# systemPackages ---------------------------------------------------------------

#' Names of Base R Packages
#' 
#' @param set_number integer number specifying a set of packages: 1 or 2.
#' @return vector of character representing package names
#' @export
#' 
systemPackages <- function(set_number = 1L)
{
  common <- c("stats", "graphics", "grDevices", "utils", "methods")
  
  if (set_number == 1L) {
    return(c(common, "datasets", "base"))
  } 
  
  if (set_number == 2L) {
    return(c(common, "grid", "splines", "tools"))
  }
  
  cleanStop("set_number must be one of 1, 2.")
}
