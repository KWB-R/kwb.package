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
