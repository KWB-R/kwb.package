# installedDependencies --------------------------------------------------------

#' What Versions of Package Dependencies are Installed?
#' 
#' @param package name of the package of which to check the dependencies
#' @param recursive whether to look recursively for dependencies or only for the
#'   direct dependencies of \code{package}. Passed to 
#'   \code{\link{packageDependencies}}, defaults to \code{TRUE}
#' @importFrom kwb.utils selectElements
#' @importFrom utils installed.packages
#' @export
#' @examples 
#' installedDependencies(package = "kwb.package")
#' installedDependencies(package = "kwb.package", recursive = FALSE)
installedDependencies <- function(package, recursive = TRUE)
{
  #kwb.utils::assignPackageObjects("kwb.package");recursive=TRUE
  
  # What other packages does the package depend on?
  dependencies <- sort(kwb.utils::selectElements(
    packageDependencies(package, recursive = recursive), 
    package
  ))
  
  # What versions are the required packages installed in?
  utils::installed.packages()[dependencies, "Version", drop = FALSE]
}
