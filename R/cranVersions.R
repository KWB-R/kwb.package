# cranVersions -----------------------------------------------------------------

#' @noMd
#' @noRd
#' @keywords internal
#' @importFrom kwb.utils removeColumns safeRowBind
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
    "cran_package_file", 
    package = name, 
    version = current$version
  )
  
  urlPattern <- getUrl(
    "cran_archive_file", 
    package = name, 
    package_filename = "%s"
  )
  
  archived$package_source_url <- sprintf(urlPattern = archived$archive_file)
  
  result <- kwb.utils::safeRowBind(archived, current)
  
  kwb.utils::removeColumns(result, "archive_file")
}
