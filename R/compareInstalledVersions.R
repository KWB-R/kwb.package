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
  #kwb.utils::assignPackageObjects("kwb.package")
  #x <- "~/../Downloads/S/sema-plus-simulatoren/SEMA_Datenvorbereiter_v1.1.0/lib"
  installed_versions <- function(x) {
    path.expand(x) %>% 
      utils::installed.packages() %>% 
      asNoFactorDataFrame() %>% 
      renameAndSelect(list(
        Package = "name", 
        Version = "version"
      )) %>% 
      resetRowNames()
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
