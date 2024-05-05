# archivedCranVersions ---------------------------------------------------------

#' Archived CRAN versions
#' 
#' @param package package name
#' @param ref_date  default: NULL
#' @export
#' @examples 
#' packages <- c("ggplot2", "swmmr", "kwb.hantush")
#' archivedCranVersions(packages)
#' archivedCranVersions(packages, ref_date= "2012-12-01")
archivedCranVersions <- function(package, ref_date = NULL)
{
  if (length(package) > 1L) {
    return(do.call(rbind, lapply(
      package, archivedCranVersions, ref_date = ref_date
    )))
  } 
  
  src <- readLinesFromUrl(getUrl("cran_archive", package = package))
  
  if (is.null(src)) {
    return(noFactorDataFrame(
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
  
  versions <- cbind(
    package = package, 
    extractSubstring(
      pattern = pattern,
      x = grep(pattern, src, value = TRUE), 
      index = c(
        version = 2L, 
        date = 3L, 
        archive_file = 1L
      )
    )
  )
  
  versions$date <- as.Date(versions$date)
  versions$date_type <- "last_modified"
  
  if (is.null(ref_date)) {
    return(versions)
  }
  
  getLastVersionBefore(versions, as.Date(ref_date))
}

# getLastVersionBefore ---------------------------------------------------------

#' @importFrom utils tail
getLastVersionBefore <- function(versions, ref_date)
{
  X = unname(splitBy(versions, "package"))
  
  last_before <- function(x) utils::tail(x[x$date <= ref_date, ], 1L)
  
  resetRowNames(do.call(rbind, lapply(X, last_before)))
}
