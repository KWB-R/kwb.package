# loadDescriptionFromWeb -------------------------------------------------------
#' @noMd
#' @noRd
#' @keywords internal
#' @importFrom gh gh 
loadDescriptionFromWeb <- function(
  name, 
  version = NA, 
  github_user = "KWB-R", 
  path = tempdir(),
  cache = list(descriptions = list(), versions = list())
)
{
  #kwb.utils::assignPackageObjects("kwb.package")
  #kwb.utils::assignArgumentDefaults(loadDescriptionFromWeb)
  #name = "sema.berlin";version = "1.6.1";github_user = "KWB-R"
  
  # Try to load DESCRIPTION from cache
  key <- paste(name, version, sep = ":")
  description <- cache$descriptions[[key]]
  
  if (! is.null(description)) {
    return(description)
  }
  
  # Try to load package versions from cache
  versions <- cache$versions[[name]]
  
  if (! is.null(versions)) {
    cleanStop("loadDescriptionFromWeb(): !is.null(versions) not implemented!")
    #description <- select_version(cran_versions)
  } else {
    versions <- cranVersions(name, dbg = FALSE)
  }
  
  if (! is.null(versions)) {
    
    version <- defaultIfNa(version, rev(versions$version)[1L])
    
    if (! version %in% versions$version) {
      cleanStop(noSuchElements(version, versions$version, "version"))
    }
    
    url <- versions$package_source_url[versions$version == version]
    
    return(loadDescriptionFromArchiveUrl(url, path))
  }
  
  # Look on KWB's Github account
  repo <- githubRepo(github_user, name)
  
  sha <- if (is.na(version)) {
    
    "master"
    
  } else {
    
    versions <- githubPackageVersions(repo, reduced = FALSE)
    shas <- selectColumns(versions, c("version", "sha"))
    
    if (! version %in% shas$version) {
      cleanStop(noSuchElements(version, shas$version, "version"))
    }
    
    shas$sha[shas$version == version]
  }
  
  readGithubPackageDescription(repo, sha)
}
