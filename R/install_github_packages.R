
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

#' Install GitHub Packages 
#'
#' @param lib path to R library where packages should be instaslled
#' @param pkgs_full_name vector with full sname to GitHub R packages (e.g. "kwb-r/kwb.utils")
#' @param dependencies passed to remotes::install_github(). TRUE is shorthand for 
#' "Depends", "Imports", "LinkingTo" and "Suggests"  NA is shorthand for "Depends", 
#' "Imports" and "LinkingTo" and is the default. FALSE is shorthand for no 
#' dependencies (i.e. just check this package, not its dependencies), (default: TRUE)
#' @param upgrade  passed to remotes::install_github(), (default: "always")
#' @param auth_token GitHub Personal Access token, required with scope "private" if
#' access to non-public R packages is required (default: Sys.getenv("GITHUB_PAT"))
#' @return installs multiple GitHub R packages into one R library
#' @export
#' @importFrom fs dir_create
#' @importFrom withr with_libpaths
#' @importFrom remotes install_github
#' @examples
#' \dontrun{
#' remotes::install_github("kwb-r/pkgmeta") 
#' pkgs <- pkgmeta::get_github_packages()
#' paths_list <- list(
#' r_version = kwb.packages::getRVersionMajorMinor(),
#' lib_linux = "/usr/lib/R/site-library",
#' lib_win = "<win_root_dir>/kwbran/<r_version>"
#' )
#' 
#' paths <- kwb.utils::resolve(paths_list,
#' win_root_dir = fs::path(tempdir())) 
#' 
#' pkgs <- pkgmeta::get_github_packages()
#' 
#' installGithubPackages(lib = paths$lib_win, pkgs$full_name)
#' installGithubPackages(lib = paths$lib_linux, pkgs$full_name)
#' }
#' 
installGithubPackages <- function(
  lib, 
  pkgs_full_name, 
  dependencies = TRUE, 
  upgrade = "always", 
  auth_token = Sys.getenv("GITHUB_PAT")
)
{
  fs::dir_create(lib, recurse = TRUE)
  
  withr::with_libpaths(new = lib, {
    install.packages("remotes", repos = "https://cran.rstudio.org")
  })
  
  for (full_name in pkgs_full_name) {
    
    withr::with_libpaths(new = lib, {
      
      code = remotes::install_github(
        repo = full_name,
        dependencies = dependencies,
        upgrade = upgrade,
        auth_token = auth_token
      )
    })
  }
}
