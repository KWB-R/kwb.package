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
  descriptionFile <- tempfile("DESCRIPTION_")
  on.exit(unlink(descriptionFile))
  
  writeLines(
    text = strsplit(selectElements(content, "message"), "\r?\n")[[1L]], 
    con = descriptionFile
  )
    
  # Read local DESCRIPTION file
  description <- readDescriptionAddingPath(descriptionFile)
  
  # Use package name and version to generate a name for the cached
  # DESCRIPTION file. Copy the DESCRITPION file to a file of that name.
  copiedFile <- copyFile(
    from = descriptionFile, 
    to = file.path(
      destdir, 
      getUrl("cached_desc", package_and_version = sprintf(
        "%s_%s", 
        selectElements(description, "package"), 
        selectElements(description, "version")
      ))
    )
  )
  
  # Save the URL to the description file in the description object
  description$url <- endpoint
  
  description
}
