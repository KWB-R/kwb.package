
#' Helper: Get R major minor version string 
#'
#' @return returns R version major.minor string (e.g. 4.0), used by standard R 
#' libraries for grouping all R packages into one folder
#' @export
#' @importFrom utils sessionInfo
#' @importFrom stringr str_extract
#' @examples
#' get_r_version_majorminor(
#' )
get_r_version_majorminor <- function() {
  
  session_metadata <- sessionInfo()
  
  paste0(session_metadata$R.version$major, ".",
         stringr::str_extract(session_metadata$R.version$minor, 
                              "^[0-9]+"))
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
#' r_version = kwb.packages::get_r_version_majorminor(),
#' lib_linux = "/usr/lib/R/site-library",
#' lib_win = "<win_root_dir>/kwbran/<r_version>"
#' )
#' 
#' paths <- kwb.utils::resolve(paths_list,
#' win_root_dir = fs::path(tempdir())) 
#' 
#' pkgs <- pkgmeta::get_github_packages()
#' 
#' install_kwb_github_packages(lib = paths$lib_win, pkgs$full_name)
#' install_kwb_github_packages(lib = paths$lib_linux, pkgs$full_name)
#' }
#' 
install_github_packages <- function(lib, 
                                    pkgs_full_name,
                                    dependencies = TRUE,
                                    upgrade = "always",  
                                    auth_token = Sys.getenv("GITHUB_PAT")) {
  
  fs::dir_create(lib, recurse = TRUE)
  withr::with_libpaths(new = lib,  {
    install.packages("remotes", repos = "https://cran.rstudio.org")  }
  )
  for(full_name in pkgs_full_name) {
    withr::with_libpaths(new = lib, {
      code = remotes::install_github(repo = full_name,
                                     dependencies = dependencies,
                                     upgrade = upgrade,
                                     auth_token = auth_token)})
  }
  
}
