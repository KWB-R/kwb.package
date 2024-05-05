# readGithubPackageDescription -------------------------------------------------
 
#' Read DESCRIPTION File for R Package on GitHub
#' 
#' @param repo GitHub repository, e.g. "kwb-r/kwb.utils"
#' @param sha SHA (hash) of the commit 
#' @param auth_token GitHub token
#' @param destdir path to destination directory, i.e. directory to which the
#'   DESCRIPTION file is copied. Default: \code{tempdir()}
#' @importFrom gh gh
readGithubPackageDescription <- function(
  repo, sha, auth_token = remotes_github_pat(), destdir = tempdir()
)
{
  endpoint <- getUrl("github_desc", repo = repo, sha = sha)
  content <- try(gh::gh(endpoint, .token = auth_token), silent = TRUE)
  
  if (inherits(content, "try-error")) {
    message("Error: ", content)
    return(NULL)
  }

  # Save to local DESCRIPTION file
  file <- tempfile()
  on.exit(unlink(file))
  
  contentLines <- strsplit(selectElements(content, "message"), "\r?\n")[[1L]]
                           
  writeLines(contentLines, file)
  
  # Read local DESCRIPTION file
  desc <- remotes_read_dcf(file)

  # Use package name and version to generate a name for the cached
  # DESCRIPTION file. Copy the DESCRITPION file to a file of that name.
  
  file.copy(file, file.path(destdir, getUrl(
    "cached_desc", 
    package = selectElements(desc, "Package"), 
    version = selectElements(desc, "Version")
  )))
  
  # See remotes:::load_pkg_description
  names(desc) <- tolower(names(desc))
  desc$path <- endpoint
  
  desc
}
