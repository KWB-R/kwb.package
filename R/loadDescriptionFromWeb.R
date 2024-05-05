# loadDescriptionFromWeb -------------------------------------------------------

#' Load Package DESCRIPTION from a File in the Internet
#' 
#' @param name package name
#' @param version version string. Default: NA
#' @param github_user Default: "KWB-R"
#' @param path path to local .tar.gz file
#' @param cache list with elements "descriptions", "versions", used as a cache
#' @export
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
  #name = "ggplot2";version = "2.1.0";github_user = "KWB-R"
  
  # Try to load DESCRIPTION from cache
  key <- paste(name, version, sep = ":")
  description <- selectElements(cache, "descriptions")[[key]]
  
  if (!is.null(description)) {
    return(description)
  }
  
  # Try to load package versions from cache
  versions <- selectElements(cache, "versions")[[name]]
  
  if (!is.null(versions)) {
    cleanStop("loadDescriptionFromWeb(): !is.null(versions) not implemented!")
    #description <- select_version(cran_versions)
  } else {
    versions <- cranVersions(name, dbg = FALSE)
  }
  
  if (!is.null(versions)) {
    
    versionNumbers <- selectColumns(versions, "version")
    version <- defaultIfNa(version, rev(versionNumbers)[1L])
    
    if (!version %in% versionNumbers) {
      cleanStop(noSuchElements(version, versionNumbers, "version"))
    }
    
    isVersion <- versionNumbers == version
    url <- selectColumns(versions, "package_source_url")[isVersion]
    
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

