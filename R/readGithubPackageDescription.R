# readGithubPackageDescription -------------------------------------------------
readGithubPackageDescription <- function(
  repo, sha, auth_token = remotes:::github_pat()
)
{
  endpoint <- getUrl("github_desc", repo = repo, sha = sha)
  content <- try(gh::gh(endpoint, .token = auth_token), silent = TRUE)
  
  if (inherits(content, "try-error")) {
    return(NULL)
  }

  txt <- kwb.utils::selectElements(content, "message")

  # Read DESCRIPTION from character instead of file
  read_dcf <- function(...) {
    con <- textConnection(txt)
    on.exit(close(con))
    read.dcf(con, ...)
  }
  
  # See remotes:::read_dcf
  fields <- colnames(read_dcf())
  desc <- as.list(read_dcf(keep.white = fields)[1L, ])
  
  # See remotes:::load_pkg_description
  names(desc) <- tolower(names(desc))
  desc$path <- endpoint
  
  desc
}
