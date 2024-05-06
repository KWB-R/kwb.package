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
