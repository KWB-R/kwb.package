# isOnCran ---------------------------------------------------------------------
isOnCran <- function(name)
{
  nrow(currentCranVersion(name)) > 0L
}

# cranVersions -----------------------------------------------------------------
cranVersions <- function(name, dbg = TRUE)
{
  current <- currentCranVersion(name)
    
  if (nrow(current) == 0L) {
    
    if (dbg) {
      message(sprintf(
        "Package '%s' does not seem to be on CRAN.", name
      ))
    }
    
    return(NULL)
  }

  archived <- archivedCranVersions(name)

  current$package_source_url <- getUrl(
    "cran_package_file", package = name, version = current$version
  )
  
  archived$package_source_url <- sprintf(
    getUrl("cran_archive_file", package = name, package_filename = "%s"), 
    archived$archive_file
  )
  
  result <- kwb.utils::safeRowBind(archived, current)
  
  kwb.utils::removeColumns(result, "archive_file")
}

# currentCranVersion -----------------------------------------------------------
currentCranVersion <- function(name)
{
  src <- readLinesFromUrl(getUrl("cran_package", package = name))
  
  was_removed_pattern <- "was removed from the CRAN repository"
  
  if (is.null(src) || any(grepl(was_removed_pattern, src))) {
    return(kwb.utils::noFactorDataFrame(
      package = character(0L),
      version = character(0L),
      date = as.Date(character(0L)),
      date_type = character(0L)
    ))
  }
  
  extract <- function(x) {
    gsub("<td>|</td>", "", src[grep(sprintf("<td>%s:</td>", x), src) + 1L])
  }
  
  kwb.utils::noFactorDataFrame(
    package = name,
    version = extract("Version"),
    date = as.Date(extract("Published")),
    date_type = "published"
  )
}

# archivedCranVersions ---------------------------------------------------------
# packages <- c("ggplot2", "swmmr", "kwb.hantush")
# archivedCranVersions(packages)
# archivedCranVersions(packages, ref_date= "2012-12-01")
archivedCranVersions <- function(package, ref_date = NULL)
{
  if (length(package) > 1L) {
    return(do.call(rbind, lapply(
      package, archivedCranVersions, ref_date = ref_date
    )))
  } 
  
  src <- readLinesFromUrl(getUrl("cran_archive", package = package))

  if (is.null(src)) {
    return(kwb.utils::noFactorDataFrame(
      package = character(0L),
      version = character(0L),
      date = as.Date(character(0L)),
      archive_file = character(0L),
      date_type = character(0L)
    ))
  }
  
  pattern <- sprintf(
    "href=\"(%s_(.*)\\.tar\\.gz)\".*(\\d{4}-\\d{2}-\\d{2}) ", package
  )
  
  versions <- cbind(package = package, kwb.utils::extractSubstring(
    pattern = pattern,
    x = grep(pattern, src, value = TRUE), 
    index = c(
      version = 2L, 
      date = 3L, 
      archive_file = 1L
    )
  ))

  versions$date <- as.Date(versions$date)
  versions$date_type <- "last_modified"
  
  if (is.null(ref_date)) {
    return(versions)
  }
  
  getLastVersionBefore(versions, as.Date(ref_date))
}

# getLastVersionBefore ---------------------------------------------------------
getLastVersionBefore <- function(version_dates, ref_date)
{
  X = unname(split(version_dates, version_dates$package))
  
  last_before <- function(x) utils::tail(x[x$date <= ref_date, ], 1L)
  
  kwb.utils::resetRowNames(do.call(rbind, lapply(X, last_before)))
}
