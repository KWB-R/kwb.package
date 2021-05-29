# githubPackageVersions --------------------------------------------------------
# Haukes version of github_packages_versions()
githubPackageVersions <- function(
  repo,
  auth_token = remotes:::github_pat(),
  verbose = TRUE
)
{
  stopifnot(is.character(repo))

  if (length(repo) > 1L) {
    return(do.call(rbind, lapply(
      repo,
      githubPackageVersions,
      auth_token = auth_token,
      verbose = verbose
    )))
  }

  if (verbose) {
    message("Reading ", repo)
  }

  # Shortcut
  get <- kwb.utils::selectColumns

  # Endpoint to raw DESCRIPTION file of a certain commit
  description_url <- function(repo, sha) sprintf(
    "https://raw.githubusercontent.com/%s/%s/DESCRIPTION", repo, sha
  )

  # Get release information (may be NULL if there are no releases)
  result <- getGithubReleaseInfo(repo, reduced = FALSE, auth_token = auth_token)

  # Return NULL if there are no releases
  if (is.null(result)) {
    return(NULL)
  }

  # Read the description files of the commits referred to by the releases
  descriptions <- lapply(
    get(result, "sha"),
    readGithubPackageDescription,
    repo = repo,
    auth_token = auth_token
  )

  result$package <- basename(result$repo)

  result$remote <- sprintf("github::%s@%s", result$repo, result$tag)

  result$version <- sapply(descriptions, function(x) {
    if (! "Version" %in% colnames(x)) {
      return(NA_character_)
    }
    unname(x[, "Version"])
  })

  result <- kwb.utils::removeColumns(result, c("sha", "repo", "tag", "release"))
  result <- kwb.utils::moveColumnsToFront(result, c("package", "version", "date"))

  result <- result[! is.na(result$date) & ! is.na(result$version), ]

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
  content <- try(gh::gh(endpoint, .token = auth_token), silent = TRUE)

  if (inherits(content, "try-error")) {
    return(NULL)
  }

  con <- textConnection(kwb.utils::selectElements(content, "message"))
  on.exit(close(con))

  read.dcf(con)
}
