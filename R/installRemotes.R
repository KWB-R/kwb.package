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
