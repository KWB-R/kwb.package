# downloadGitHubPackage --------------------------------------------------------

#' Download an R Package from GitHub
#' 
#' @param repo path to repository, relative to https://github,com, e.g. 
#'   "kwb-r/kwb.utils"
#' @param destdir path to download folder, default: "~/../Downloads"
#' @return path to downloaded file in the \code{destdir} folder with attribute 
#'   "origin" pointing to the original file in \code{tempdir()}.
#' @export
downloadGitHubPackage <- function(repo, destdir = "~/../Downloads")
{
  # Extract package name from repo string (remove everything before and 
  # including "/", and everything after and including "@")
  package <- gsub("^.*/|@[^@]+$", "", repo)

  file_downloaded <- packageInDestdir(package, destdir)
  
  if (file_downloaded) {

    file <- kwb.utils::getAttribute(file_downloaded, "path")
    origin <- character(0)
    
  } else{
    
    result <- try(remotes:::remote_download(
      remotes:::github_remote(repo), 
      quiet = FALSE
    ))
    
    if (inherits(result, "try-error")) {
      
      file <- character(0)
      origin <- character(0)
      
    } else {
      
      file <- file.path(destdir, findPackageFilename(tarfile = result)                        )
      file.copy(result, file, overwrite = TRUE)
      origin <- result
    }
  }
  
  structure(file, origin = origin)
}

# findPackageFilename ----------------------------------------------------------
findPackageFilename <- function(tarfile)
{
  paths <- utils::untar(tarfile, list = TRUE)
  
  desc_file <- grep("/DESCRIPTION$", paths, value = TRUE)
  
  if (length(desc_file) == 0L) {
    stop("No DESCRIPTION found in ", tarfile)
  }
  
  utils::untar(tarfile, files = desc_file, exdir = tempdir())
  
  package_info <- read.dcf(file.path(tempdir(), desc_file))
  
  sprintf("%s_%s.tar.gz", package_info[, "Package"], package_info[, "Version"])
}
