#' readGithubPackageDescription ------------------------------------------------
#' @noMd
#' @noRd
#' @keywords internal
#' @importFrom gh gh
#' @importFrom kwb.utils selectElements
readGithubPackageDescription <- function(
  repo, sha, auth_token = remotes:::github_pat(), destdir = tempdir()
)
{
  endpoint <- getUrl("github_desc", repo = repo, sha = sha)
  content <- try(gh::gh(endpoint, .token = auth_token), silent = TRUE)
  
  if (inherits(content, "try-error")) {
    return(NULL)
  }

  # Save to local DESCRIPTION file
  file <- tempfile()
  on.exit(unlink(file))
  
  writeLines(kwb.utils::selectElements(content, "message"), file)
  
  # Read local DESCRIPTION file
  desc <- remotes:::read_dcf(file)

  # Use package name and version to generate a name for the cached
  # DESCRIPTION file. Copy the DESCRITPION file to a file of that name.
  get <- kwb.utils::createAccessor(desc)
  file.copy(file, file.path(destdir, getUrl(
    "cached_desc", package = get("Package"), version = get("Version")
  )))
  
  # See remotes:::load_pkg_description
  names(desc) <- tolower(names(desc))
  desc$path <- endpoint
  
  desc
}
