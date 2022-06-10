# loadDescriptionFromWeb -------------------------------------------------------
#' @noMd
#' @noRd
#' @keywords internal
#' @importFrom kwb.utils defaultIfNA selectColumns
#' @importFrom gh gh 
loadDescriptionFromWeb <- function(
  name, version = NA_character_, github_user = "KWB-R"
)
{
  #name = "sema.berlin";version = "1.6.1";github_user = "KWB-R"
  if (isOnCran(name)) {
    
    versions <- cranVersions(name)
    
    version <- kwb.utils::defaultIfNA(version, rev(versions$version)[1L])
    
    if (! version %in% versions$version) {
      stop_(kwb.utils:::noSuchElements(version, versions$version, "version"))
    }
    
    url <- versions$package_source_url[versions$version == version]
    
    return(loadDescriptionFromArchiveUrl(url))
  }
  
  # Look on KWB's Github account
  repo <- githubRepo(github_user, name)
  
  sha <- if (is.na(version)) {
    
    "master"
    
  } else {
    
    versions <- githubPackageVersions(repo, reduced = FALSE)
    shas <- kwb.utils::selectColumns(versions, c("version", "sha"))
    
    if (! version %in% shas$version) {
      stop_(kwb.utils:::noSuchElements(version, shas$version, "version"))
    }
    
    shas$sha[shas$version == version]
  }
  
  readGithubPackageDescription(repo, sha)
}
