# updateKwbPackages ------------------------------------------------------------

#' Update or Install KWB-Packages
#' 
#' Update installed KWB-packages or install KWB-packages for the first time
#' 
#' @param packageNames vector of packages to be installed. Default: names of all
#'   installed KWB-packages
#' @param skip vector of packages not to be installed, even if they are listed
#'   in \emph{packageNames}
#' @param package.dir full path to the folder containing the binary package 
#'   files
#'   
#' @export
#' 
updateKwbPackages <- function
(
  packageNames = sort(installedKwbPackages()),
  skip = character(),
  package.dir = defaultPackageDir()
)
{
  # Vector of packages that have already been handled
  handled <- character()

  cat("*** Package directory:", package.dir, "\n")

  while (length(packageNames) > 0) {

    packageName <- packageNames[1]

    installPackageIfRequired(packageName, package.dir, skip)

    handled <- c(handled, packageName)

    packageNames <- c(packageNames, packageDependencies(packageName)[[1]])

    packageNames <- setdiff(packageNames, handled)
  }
}

# getServername ------------------------------------------------------------

#' Get KWB Servername
#' 
getServername <- function()
{
  variable <- "SERVERNAME"
  
  servername <- Sys.getenv(variable)
  
  if (servername == "") {
    stopFormatted(
      paste(
        "Enviroment variable '%s' not defined!",
        "Please define with Sys.setenv(%s = 'kwb-servername')", 
        sep = "\n"
      ),
      variable,
      variable
    )
  }
  
  servername
}

# defaultPackageDir ------------------------------------------------------------

#' Default Package Directory
#' 
defaultPackageDir <- function()
{
  sprintf(
    "//%s/miacso$/REvaluation/RPackages/kwb.packages",
    getServername()
  )
}

# installPackageIfRequired -----------------------------------------------------

installPackageIfRequired <- function(packageName, package.dir, skip, ...)
{
  cat("Required package:", packageName, "...\n  ")

  if (shallBeInstalled(packageName, skip)) {
    
    installPackage(packageName, package.dir, ...)
    
  } else {
    
    cat("skipped.\n")
    dependentPackages <- NULL
  }
}

#' shallBeInstalled ------------------------------------------------------------
#' @noMd
#' @noRd
#' @keywords internal
#' @importFrom utils installed.packages

shallBeInstalled <- function(packageName, skip)
{
  # Install only if
  # - the package is not in the list of packages to be skipped and
  # - if the package is either a kwb package or another package that is not yet
  #   installed

  toBeSkipped <- packageName %in% skip
  installed <- packageName %in% utils::installed.packages()[, "Package"]

  ! toBeSkipped && (isKwbPackage(packageName) || ! installed)
}

#' installPackage --------------------------------------------------------------
#' @noMd
#' @noRd
#' @keywords internal
#' @importFrom utils install.packages
#' @importFrom remotes install_github
installPackage <- function(
  packageName, package.dir = defaultPackageDir(), quiet = TRUE, type = NULL, 
  dbg = FALSE
)
{
  #kwb.utils::assignPackageObjects("kwb.package")
  #package.dir = defaultPackageDir(); quiet = TRUE; type = NULL; dbg = FALSE

  if (isKwbPackage(packageName)) {

    packageFile <- getPackageFilesToInstall(
      package.dir, packageName, dbg = dbg, warn = FALSE
    )

    if (length(packageFile) == 0) {
      
      message(
        "No package file found for package '", packageName, "'. ",
        "Trying to find it at github... "
      )
      
      try(remotes::install_github(paste0("kwb-r/", packageName)))
      
    } else if (length(packageFile) > 1) {
      
      warning(
        "I found more than one file for package ", packageName, ":",
        paste(packageFile, collapse = ", ")
      )
      
    } else {
      
      packageFile <- packageFile[1]
      repos <- NULL
    }
    
  } else {
    
    packageFile <- packageName
    repos <- getOption("repos")
  }

  if (length(packageFile) == 1) {
    
    cat("Installing", basename(packageFile), "... ")
    
    if (is.null(type)) {
      
      type <- ifelse(grepl("\\.tar\\.gz$", packageFile), "source", "binary")
    }

    utils::install.packages(
      packageFile, repos = repos, quiet = quiet, type = type
    )
    
    cat("ok.\n")
  }
}

# getPackageFilesToInstall -----------------------------------------------------

#' Get Package Files to Install
#' 
#' Get paths/names of package files available in a directory
#' 
#' @param package.dir full path to directory containing the package files
#' @param packageNames optional vector of character containing the names of the
#'   packages to be installed. If NULL (default), all available packages in
#'   \emph{package.dir} are installed,
#' @param filepattern pattern matching names of files to be considered. Default:
#'   "^kwb\\..*\\.zip$"
#' @param full.names if TRUE (default) the full paths to the package files are
#'   returned, else only the file names
#' @param dbg  if \code{TRUE} (default) debug messages are shown
#' @param warn if \code{TRUE} (default) warnings are given if no corresponding
#'   package files are found
#'   
#' @return character vector containing the full paths to or just the names of
#'   the available package files
#'   
#' @export
getPackageFilesToInstall <- function(
  package.dir = defaultPackageDir(), packageNames = NULL, filepattern = "",
  full.names = TRUE, dbg = FALSE, warn = TRUE
)
{
  #kwb.utils::assignPackageObjects("kwb.package")
  #package.dir = defaultPackageDir(); packageNames = NULL; filepattern = "";
  #full.names = TRUE; dbg = TRUE; warn = TRUE  
  
  catIf(
    dbg, "Looking for available package files in '", package.dir, "'...\n"
  )

  availablePackageFiles <- dir(package.dir, filepattern)

  # If no package names are given, use all available package files
  if (is.null(packageNames)) {
    
    packageNames <- unique(sapply(strsplit(availablePackageFiles, "_"), "[", 1))
  }

  filesToInstall <- as.character(unlist(sapply(
    packageNames, FUN = getFileToInstall, availablePackageFiles, dbg,
    warn = warn
  )))

  if (full.names) {
    
    file.path(package.dir, filesToInstall)
    
  } else {
    
    filesToInstall
  }
}

# getFileToInstall -------------------------------------------------------------

getFileToInstall <- function(
  package, availablePackageFiles, dbg = TRUE, warn = TRUE
)
{
  pattern <- paste0("^", package, "_")

  packageFiles <- grep(pattern, availablePackageFiles, value = TRUE)

  numberOfPackageFiles <- length(packageFiles)

  catIf(dbg, "***", package, ": ")

  if (numberOfPackageFiles == 0) {

    messageText <- paste("No package file available for package", package)

    catIf(dbg, messageText, "\n")

    if (warn) {
      warning(messageText)
    }

    return (NULL)
  }

  if (numberOfPackageFiles > 1) {
    
    catIf(dbg, length(packageFiles), "package files available! ")
  }
  
  packageFile <- sortPackageFiles(packageFiles)[1]
  
  catIf(dbg, "I will import:", packageFile, "\n")

  packageFile
}

# sortPackageFiles -------------------------------------------------------------
sortPackageFiles <- function(packageFiles)
{
  #packageFile <- sort(packageFiles, decreasing = TRUE)[1]
  version_strings <- gsub("^[^_]+_((\\d+\\.)+).*$", "\\1", packageFiles)
  
  version_strings <- substr(version_strings, 1, nchar(version_strings) - 1)
  
  version_order <- order(numeric_version(version_strings), decreasing = TRUE)
  
  packageFiles[version_order]
}

# installedKwbPackages ---------------------------------------------------------

#' Installed KWB-Packages
#' 
#' @return vector of names of installed kwb-packages
#' 
#' @export
#' @importFrom utils installed.packages
installedKwbPackages <- function()
{
  packageNames <- utils::installed.packages()[, "Package"]

  as.character(packageNames[isKwbPackage(packageNames)])
}

# isKwbPackage -----------------------------------------------------------------

isKwbPackage <- function(packageName)
{
  grepl("^kwb", packageName)
}
