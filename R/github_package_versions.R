# githubPackageVersions --------------------------------------------------------
githubPackageVersions <- function(repo)
{
  #repo <- "KWB-R/sema.berlin"

  # Shortcut
  get <- kwb.utils::selectColumns
  
  description_url <- function(repo, sha) sprintf(
    "https://raw.githubusercontent.com/%s/%s/DESCRIPTION", repo, sha
  )
  
  result <- getGithubReleaseInfo(repo, reduced = FALSE)

  if (is.null(result)) {
    return(NULL)
  }
  
  descriptions <- lapply(
    get(result, "sha"), 
    readGithubPackageDescription,
    repo = repo
  )
  
  result$package <- basename(result$repo)
  
  result$remote <- sprintf("github::%s@%s", result$repo, result$tag)
  
  result$version <- sapply(descriptions, function(x) {
    stopifnot("Version" %in% colnames(x))
    unname(x[, "Version"])
  })

  result <- kwb.utils::removeColumns(result, c("sha", "repo", "tag", "release"))
  result <- kwb.utils::moveColumnsToFront(result,c("package", "version", "date"))
  
  result <- result[! is.na(result$date), ]
  
  kwb.utils::orderBy(result, "date")
}

# getGithubReleaseInfo ---------------------------------------------------------
getGithubReleaseInfo <- function(
  repo, reduced = TRUE, auth_token = remotes:::github_pat()
)
{
  # Shortcut
  get <- kwb.utils::selectElements
  
  releases_url <- function(repo) sprintf(
    "https://api.github.com/repos/%s/releases", repo
  )

  tags_url <- function(repo) sprintf(
    "https://api.github.com/repos/%s/tags", repo
  )

  get_endpoint <- function(endpoint) {
    stopifnot(length(endpoint) == 1L)
    gh::gh(endpoint, .token = auth_token)
  }
  
  releases <- get_endpoint(endpoint = releases_url(repo))
  tags <- get_endpoint(endpoint = tags_url(repo))

  if (length(tags) == 0L) {
    return(NULL)
  }
  
  tag_info <- kwb.utils::noFactorDataFrame(
    tag = sapply(tags, get, "name"),
    sha = sapply(lapply(tags, get, "commit"), get, "sha")
  )
  
  release_info <- kwb.utils::noFactorDataFrame(
    tag = sapply(releases, get, "tag_name"),
    date = as.Date(sapply(releases, get, "published_at")),
    release = sapply(releases, get, "name"),
    author = sapply(releases, function(x) get(get(x, "author"), "login"))
  )
  
  result <- cbind(
    repo = repo,
    merge(tag_info, release_info, by = "tag", all.x = TRUE),
    stringsAsFactors = FALSE
  )
  
  if (! reduced) {
    return(result)
  }
  
  kwb.utils::removeColumns(result, "sha")
}

# readGithubPackageDescription -------------------------------------------------
readGithubPackageDescription <- function(
  repo, sha, auth_token = remotes:::github_pat()
)
{
  description_url <- function(repo, sha) sprintf(
    "https://raw.githubusercontent.com/%s/%s/DESCRIPTION", repo, sha
  )

  endpoint <- description_url(repo, sha)
  con <- textConnection(gh::gh(endpoint, .token = auth_token)$message)
  on.exit(close(con))
  read.dcf(con)
}
