# archived_cran_versions -------------------------------------------------------
# archived_cran_versions(c("ggplot2", "swmmr", "kwb.hantush"))
archived_cran_versions <- function(package)
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
  
  cbind(package = package, kwb.utils::extractSubstring(
    pattern = pattern,
    x = grep(pattern, html, value = TRUE), 
    index = c(version = 1L, date = 2L)
  ))
}
