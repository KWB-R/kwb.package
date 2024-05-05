# githubVersions ---------------------------------------------------------------

#' Get Versions of Packages on GitHub
#'
#' @param name package name
#' @param github_user name of github account, default: "KWB-R"
#'
#' @return data frame with one row per available version
#' @export
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

  formattedMessageIf(verbose, "Reading %s", repo)

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
    selectColumns(result, "sha"),
    readGithubPackageDescription,
    repo = repo,
    auth_token = auth_token
  )

  result$package <- basename(result$repo)

  result$remote <- sprintf("github::%s@%s", result$repo, result$tag)

  result$version <- sapply(descriptions, selectElements, "version")

  if (reduced) {
    result <- removeColumns(result, extra_columns)
  }

  result <- moveColumnsToFront(result, key_columns)

  result <- result[! is.na(result$date) & ! is.na(result$version), ]

  orderBy(result, "date")
}

# getGithubReleaseInfo ---------------------------------------------------------
#' @noMd
#' @noRd
#' @keywords internal
#' @importFrom gh gh 
getGithubReleaseInfo <- function(
  repo, reduced = TRUE, auth_token = remotes_github_pat()
)
{
  get_endpoint <- function(endpoint) {
    stopifnot(length(endpoint) == 1L)
    gh::gh(endpoint, .token = auth_token)
  }

  releases <- get_endpoint(getUrl("github_releases", repo = repo))
  tags <- get_endpoint(getUrl("github_tags", repo = repo))

  if (length(tags) == 0L) {
    return(NULL)
  }

  tag_info <- noFactorDataFrame(
    tag = sapply(tags, selectElements, "name"),
    sha = sapply(lapply(tags, selectElements, "commit"), selectElements, "sha")
  )

  release_info <- if (length(releases)) {
    noFactorDataFrame(
      tag = sapply(releases, selectElements, "tag_name"),
      date = as.Date(sapply(releases, selectElements, "published_at")),
      release = sapply(releases, selectElements, "name"),
      author = sapply(
        releases, 
        function(x) selectElements(selectElements(x, "author"), "login")
      )
    )
  } else {
    noFactorDataFrame(
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

  removeColumns(result, "sha")
}
