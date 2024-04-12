# compareInstalledVersions -----------------------------------------------------

#' Compare Package Versions Between Libraries
#' 
#' @param lib1 path to first R library
#' @param lib2 path to second R library
#' @return data frame with columns \code{name} (package name), \code{version.1},
#'   \code{version.2} (version string of package in \code{lib1} and \code{lib2},
#'   respectively), \code{differs} (logical indicating whether the version is
#'   different in the two libraries)
#' @export
#' @importFrom utils installed.packages
compareInstalledVersions <- function(lib1, lib2)
{
  installed_versions <- function(x) {
    versions <- utils::installed.packages(path.expand(x))
    resetRowNames(noFactorDataFrame(
      name = rownames(versions), 
      version = versions[, "Version", drop = FALSE]
    ))
  }
  
  versions <- merge(
    x = installed_versions(lib1), 
    y = installed_versions(lib2), 
    by = "name", 
    all = TRUE
  )
  
  names(versions)[-1L] <- c("version.1", "version.2")
  
  versions$differs <- (versions[[2L]] != versions[[3L]])
  
  versions[order(versions$name), , drop = FALSE]
}
