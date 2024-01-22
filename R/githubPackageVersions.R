# githubVersions ---------------------------------------------------------------
#' Title
#'
#' @param name package name
#' @param github_user name of github account, default: "KWB-R"
#'
#' @return data frame with one row per available version
#' @export
#' @importFrom kwb.utils moveColumnsToFront removeColumns selectColumns orderBy
#' @examples
#' githubVersions("kwb.utils")
githubVersions <- function(name, github_user = "KWB-R")
{
  githubPackageVersions(repo = githubRepo(github_user, name))
}

# githubPackageVersions --------------------------------------------------------
# Haukes version of github_packages_versions()
githubPackageVersions <- function(
  repo,
  auth_token = remotes_github_pat(),
  verbose = TRUE,
  reduced = TRUE
)
{
  #kwb.utils::assignPackageObjects("kwb.package")
  #repo = "cran/kwb.hantush";verbose=TRUE;reduced=TRUE
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
  
  # Column sets
  key_columns <- c("package", "version", "date")
  extra_columns <- c("sha", "repo", "tag", "release")
  
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

  result$version <- sapply(descriptions, kwb.utils::selectElements, "version")

  if (reduced) {
    result <- kwb.utils::removeColumns(result, extra_columns)
  }

  result <- kwb.utils::moveColumnsToFront(result, key_columns)

  result <- result[! is.na(result$date) & ! is.na(result$version), ]

  kwb.utils::orderBy(result, "date")
}

# getGithubReleaseInfo ---------------------------------------------------------
#' @noMd
#' @noRd
#' @keywords internal
#' @importFrom kwb.utils noFactorDataFrame removeColumns selectElements
#' @importFrom gh gh 
getGithubReleaseInfo <- function(
  repo, reduced = TRUE, auth_token = remotes_github_pat()
)
{
  # Shortcut
  get <- kwb.utils::selectElements

  get_endpoint <- function(endpoint) {
    stopifnot(length(endpoint) == 1L)
    gh::gh(endpoint, .token = auth_token)
  }

  releases <- get_endpoint(getUrl("github_releases", repo = repo))
  tags <- get_endpoint(getUrl("github_tags", repo = repo))

  if (length(tags) == 0L) {
    return(NULL)
  }

  tag_info <- kwb.utils::noFactorDataFrame(
    tag = sapply(tags, get, "name"),
    sha = sapply(lapply(tags, get, "commit"), get, "sha")
  )

  release_info <- if (length(releases)) {
    kwb.utils::noFactorDataFrame(
      tag = sapply(releases, get, "tag_name"),
      date = as.Date(sapply(releases, get, "published_at")),
      release = sapply(releases, get, "name"),
      author = sapply(releases, function(x) get(get(x, "author"), "login"))
    )
  } else {
    kwb.utils::noFactorDataFrame(
      tag = character(0L),
      date = as.Date(character(0L)),
      release = character(0L),
      author = character(0L)
    )
  }

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

