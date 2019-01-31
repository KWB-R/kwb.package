# initLocalCRAN ----------------------------------------------------------------

#' Create the folder structure for a local CRAN-like repository
#' 
#' @param local_cran full path to the folder representing the local CRAN 
#' 
#' @export
#' 
initLocalCRAN <- function(local_cran)
{
  dir.create(file.path(local_cran, "src", "contrib"), recursive = TRUE)
  
  # Binary paths are versioned based on R -- we'll create a path
  # for our current version of R, but leave it empty.
  
  for (path in getBinaryPaths(local_cran)) {
    
    dir.create(path, recursive = TRUE)
  }
}

# defaultLocalCRAN -------------------------------------------------------------
defaultLocalCRAN <- function(drive_letter = FALSE)
{
  # Since there are problems with the network path //moby/miacso$ we need to
  # map this network path to a drive letter (here: U:)
  
  file.path(ifelse(drive_letter, "U:", "\\\\moby/miacso$"), "local-cran")
}

# getBinaryPaths ---------------------------------------------------------------
getBinaryPaths <- function(local_cran = defaultLocalCRAN())
{
  paths <- c(
    win.binary = "bin/windows/contrib",
    mac.binary = "bin/macosx/contrib",
    mac.binary.mavericks = "bin/macosx/mavericks/contrib",
    mac.binary.leopard = "bin/macosx/leopard/contrib"
  )
  
  rVersion <- paste(unlist(getRversion())[1:2], collapse = ".")
  
  file.path(local_cran, paths, rVersion)
}

# setOptionsForPackrat ---------------------------------------------------------

#' Set Options for Using Packrat
#' 
#' Add the path to the local repository to the option "repos" and set the option
#' "pkgType" to "source". 
#' 
#' @export
#' 
#' @return The old options are returned invisibly.
#' 
setOptionsForPackrat <- function()
{
  old_options <- options()
  
  repos <- getOption("repos")
  
  if (! "local" %in% names(repos)) {
    
    options(repos = kwb.utils::hsRestoreAttributes(
      x = c(repos, local = paste0("file://", defaultLocalCRAN())), 
      attribs = attributes(repos)
    ))
  }
  
  ## The local CRAN-like repository will _not_ serve binary packages,
  ## so we need to ensure that we only install from source there.
  options(pkgType = "source")
  
  invisible(old_options)
}

# provideInLocalCRAN -----------------------------------------------------------

#' Provide a Source Package in the Local Cran
#' 
#' @param package name of the package to be looked up in either of these
#'   locations: \code{<home>/Documents/R-Development/RPackages}, 
#'   \code{<home>/Desktop/R_Development/RPackages}
#' @param rebuild logical. If \code{TRUE} the package is rebuild before all
#'   \code{.tar.gz}-files from the parent folder of the package folder are 
#'   copied to the local CRAN folder structure
#' @param local_cran full path to the folder representing the local CRAN 
#' 
#' @export
#' 
provideInLocalCRAN <- function(
  package, rebuild = TRUE, local_cran = defaultLocalCRAN(drive_letter = TRUE)
)
{
  paths <- kwb.utils::resolve(list(
    packages_1 = "<home>/Documents/R-Development/RPackages",
    packages_2 = "<home>/Desktop/R_Development/RPackages",
    contrib = "<local_cran>/src/contrib",
    home = kwb.utils::get_homedir(), 
    local_cran = local_cran
  ))
  
  package_dir <- kwb.utils::defaultIfNULL(paths$packages_1, paths$packages_2)
  
  package_dir <- kwb.utils::safePath(package_dir, package)
  
  # Add "Repository: moby" to the DESCRIPTION file if required
  desc_file <- file.path(package_dir, "DESCRIPTION")
  
  description <- read.dcf(desc_file)
  
  if (! "Repository" %in% colnames(description)) {
    
    cat("Adding 'Repository: moby' to the DESCRIPTION file... ")
    write.dcf(cbind(description, Repository = "moby"), desc_file)
    cat("ok.\n")
    
    rebuild <- TRUE
  }
  
  # Go to the package directory and build the package
  if (isTRUE(rebuild)) {
    
    kwb.utils::runInDirectory(
      target.dir = dirname(package_dir),
      FUN = system,
      command = paste("R CMD build", package)
    )
  }
  
  # Copy the tar.gz file(s) to the 'src/contrib' sub-directory
  pattern <- paste0("^", package, ".*\\.tar.gz$")
  
  files <- dir(dirname(package_dir), pattern, full.names = TRUE)
  
  file.copy(files, kwb.utils::safePath(paths$contrib))
  
  # Write the PACKAGE files
  tools::write_PACKAGES(paths$contrib, type = "source")
  
  for (path in getBinaryPaths(kwb.utils::safePath(paths$local_cran))) {
    
    tools::write_PACKAGES(path)
  }
}
