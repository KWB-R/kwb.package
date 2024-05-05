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
    version = current$version
  )
  
  urlPattern <- getUrl(
    "cran_archive_file", 
    package = name, 
    package_filename = "%s"
  )
  
  archived$package_source_url <- sprintf(urlPattern, archived$archive_file)
  
  result <- safeRowBind(archived, current)
  
  removeColumns(result, "archive_file")
}
