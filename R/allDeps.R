# allDeps ----------------------------------------------------------------------
allDeps <- function(
  name, 
  version = NA, 
  depth = 1L, 
  max_depth = 9L,
  cache = list()
)
{
  #kwb.utils::assignPackageObjects("kwb.package");name="abc";version=NA;depth=1L;max_depth=9L;cache=list()
  
  description <- loadDescriptionFromWeb(name, version)
  versionInDescription <- selectElements(description, "version")
  
  stopifnot(is.na(version) || identical(version, versionInDescription))
  
  deps <- parsePackageDeps(description)

  if (inherits(deps, "try-error") || nrow(deps) == 0L) {
    return(NULL)
  }
  
  message("depth: ", depth)
  
  deps$depth <- depth
  deps$namever <- paste(name, version, sep = ":")
  
  if (depth == max_depth) {
    message(sprintf("maximum depth (%s) reached.", max_depth))
    return(deps)
  }
  
  child_deps <- seq_len(nrow(deps)) %>% 
    lapply(function(i) {
      allDeps(deps$name[i], deps$version[i], depth + 1L, max_depth)
    }) %>% 
    excludeNull(dbg = FALSE)
  
  if (length(child_deps) > 0L) {
    deps <- rbind(deps, do.call(rbind, child_deps))
  }
  
  deps
}

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

# cranVersions -----------------------------------------------------------------

#' Get versions of CRAN packages
#' 
#' @param name package name
#' @param dbg logical indicating whether or not to show debug messages. Default: 
#'   \code{TRUE}
#' @export
cranVersions <- function(name, dbg = TRUE)
{
  current <- currentCranVersion(name)
  
  if (nrow(current) == 0L) {
    formattedMessageIf(dbg, "Package '%s' does not seem to be on CRAN.", name)
    return(NULL)
  }
  
  archived <- archivedCranVersions(name)
  
  current$package_source_url <- getUrl(
    "cran_package_file", 
    package = name, 
    version = selectColumns(current, "version")
  )
  
  archived$package_source_url <- getUrl(
    "cran_archive_file", 
    package = name, 
    package_filename = "%s"
  ) %>% 
    sprintf(selectColumns(archived, "archive_file"))
  
  archived %>% 
    safeRowBind(current) %>% 
    removeColumns("archive_file")
}

# archivedCranVersions ---------------------------------------------------------

#' Archived CRAN versions
#' 
#' @param package package name
#' @param ref_date  default: NULL
#' @importFrom utils tail
#' @export
#' @examples 
#' packages <- c("ggplot2", "swmmr", "kwb.hantush")
#' archivedCranVersions(packages)
#' archivedCranVersions(packages, ref_date= "2012-12-01")
archivedCranVersions <- function(package, ref_date = NULL)
{
  #kwb.utils::assignPackageObjects("kwb.package");`%>%` <- magrittr::`%>%`
  
  if (length(package) > 1L) {
    return(
      package %>% 
        lapply(archivedCranVersions, ref_date = ref_date) %>% 
        do.call(what = rbind)
    )
  } 
  
  text <- "cran_archive" %>% 
    getUrl(package = package) %>% 
    readLinesFromUrl()
  
  if (is.null(text)) {
    return(noFactorDataFrame(
      package = character(0L),
      version = character(0L),
      date = as.Date(character(0L)),
      archive_file = character(0L),
      date_type = character(0L)
    ))
  }
  
  filePattern <- paste0(package, "_(.*)\\.tar\\.gz")
  datePattern <- "\\d{4}-\\d{2}-\\d{2}"
  pattern <- sprintf("href=\"(%s)\".*(%s) ", filePattern, datePattern)
  
  versions <- cbind(
    noFactorDataFrame(package = package), 
    extractSubstring(
      pattern = pattern,
      x = grep(pattern, text, value = TRUE), 
      index = c(version = 2L, date = 3L, archive_file = 1L)
    )
  )
  
  versions$date <- as.Date(versions$date)
  versions$date_type <- "last_modified"
  
  if (is.null(ref_date)) {
    return(versions)
  }
  
  # For each package, get the latest version that was from before or from the 
  # reference date
  date <- as.Date(ref_date)
  
  versions %>% 
    splitBy("package") %>% 
    unname() %>% 
    lapply(function(x) utils::tail(x[x$date <= date, ], 1L)) %>% 
    do.call(what = rbind) %>% 
    resetRowNames()
}

# loadDescriptionFromArchiveUrl ------------------------------------------------

#' @noRd
#' @noMd
#' @keywords internal 
#' @importFrom utils download.file
loadDescriptionFromArchiveUrl <- function(
    url, 
    targetDir = tempdir(), 
    quiet = TRUE
)
{
  #url <- "https://cran.r-project.org/src/contrib/Archive/ggplot2/ggplot2_2.1.0.tar.gz"
  tarGzExtension <- ".tar.gz"
  stopifnot(endsWith(url, tarGzExtension))
  
  # Download .tar.gz file
  tarball <- downloadFile(url, targetDir = tempdir())
  on.exit(unlink(tarball))
  
  # Extract DESCRIPTION from downloaded .tar.gz file
  # see remotes:::load_pkg_description
  descriptionFile <- remotes_untar_description(tarball, dir = tempdir())
  on.exit(unlink(dirname(descriptionFile), recursive = TRUE), add = TRUE)
  
  # Copy the DESCRIPTION file with a unique name to the target directory
  targetFile <- copyFile(
    from = descriptionFile, 
    to = file.path(targetDir, paste0(
      "DESCRIPTION_", 
      gsub(tarGzExtension, ".txt", basename(tarball), fixed = TRUE)
    ))
  )
  
  readDescriptionAddingPath(targetFile)
}

# parsePackageDeps -------------------------------------------------------------

#' @importFrom remotes standardise_dep
parsePackageDeps <- function(description, dependencies = NA) 
{
  types <- tolower(remotes::standardise_dep(dependencies))
  
  description[intersect(names(description), types)] %>% 
    lapply(remotes_parse_deps) %>% 
    rbindAll(nameColumn = "type")
}
