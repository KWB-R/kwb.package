# archived_cran_versions -------------------------------------------------------
# archived_cran_versions(c("ggplot2", "swmmr", "kwb.hantush"))
# archived_cran_versions(c("ggplot2", "swmmr", "kwb.hantush"), ref_date= "2012-12-01")
archived_cran_versions <- function(package, ref_date = NULL)
{
  archive_url <- function(package) paste0(
    "https://cran.r-project.org/src/contrib/Archive/", package
  )
  
  version_date_pattern <- function(package) sprintf(
    "href=\"%s_(.*)\\.tar\\.gz\".*(\\d{4}-\\d{2}-\\d{2}) ", package
  )

  if (length(package) > 1L) {
    return(do.call(rbind, lapply(package, archived_cran_versions)))
  } 
  
  url <- archive_url(package)

  html <- suppressWarnings(try(readLines(url), silent = TRUE))

  if (inherits(html, "try-error")) {
    return(data.frame(version = character(), date = character()))
  }
  
  pattern <- version_date_pattern(package)
  
  version_dates <- cbind(package = package, kwb.utils::extractSubstring(
    pattern = pattern,
    x = grep(pattern, html, value = TRUE), 
    index = c(version = 1L, date = 2L)
  ))

  version_dates$date <- as.Date(version_dates$date)
  
  if (is.null(ref_date)) {
    return(version_dates)
  }
  
  get_last_version_before(version_dates, as.Date(ref_date))
}

# get_last_version_before ------------------------------------------------------
get_last_version_before <- function(version_dates, ref_date)
{
  X = unname(split(version_dates, version_dates$package))
  
  last_before <- function(x) utils::tail(x[x$date <= ref_date, ], 1L)
  
  kwb.utils::resetRowNames(do.call(rbind, lapply(X, last_before)))
}
