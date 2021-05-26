# githubPackageVersions --------------------------------------------------------
githubPackageVersions <- function(repo)
{
  #repo <- "KWB-R/kwb.utils"
  
  description_url <- function(repo, sha) sprintf(
    "https://raw.githubusercontent.com/%s/%s/DESCRIPTION", repo, sha
  )
  
  release_info <- getGithubReleaseInfo(repo)

  descriptions <- lapply(
    release_info$sha, 
    readGithubPackageDescription,
    repo = repo
  )
  
  release_info$version <- sapply(descriptions, function(x) {
    stopifnot("Version" %in% colnames(x))
    unname(x[, "Version"])
  })
  
  release_info
}

# getGithubReleaseInfo ---------------------------------------------------------
getGithubReleaseInfo <- function(repo)
{
  # Shortcut
  get <- kwb.utils::selectElements
  
  releases_url <- function(repo) sprintf(
    "https://api.github.com/repos/%s/releases", repo
  )

  tags_url <- function(repo) sprintf(
    "https://api.github.com/repos/%s/tags", repo
  )
  
  releases <- jsonlite::read_json(releases_url(repo))  
  tags <- jsonlite::read_json(tags_url(repo))  

  tag_info <- kwb.utils::noFactorDataFrame(
    tag = sapply(tags, get, "name"),
    sha = sapply(lapply(tags, get, "commit"), get, "sha")
  )
  
  release_info <- kwb.utils::noFactorDataFrame(
    tag = sapply(releases, get, "tag_name"),
    release_name = sapply(releases, get, "name"),
    release_date = as.Date(sapply(releases, get, "published_at"))
  )
  
  merge(tag_info, release_info, by = "tag", all.x = TRUE)
}

# readGithubPackageDescription -------------------------------------------------
readGithubPackageDescription <- function(repo, sha)
{
  description_url <- function(repo, sha) sprintf(
    "https://raw.githubusercontent.com/%s/%s/DESCRIPTION", repo, sha
  )
  
  con <- file(description_url(repo, sha))
  on.exit(close(con))
  read.dcf(con)
}
