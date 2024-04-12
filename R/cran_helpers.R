# currentCranVersion -----------------------------------------------------------

#' @noMd
#' @noRd
#' @keywords internal
#' @importFrom kwb.utils noFactorDataFrame
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
    "cran_package_file", package = name, version = current$version
  )
  
  archived$package_source_url <- sprintf(
    getUrl("cran_archive_file", package = name, package_filename = "%s"), 
    archived$archive_file
  )
  
  result <- kwb.utils::safeRowBind(archived, current)
  
  kwb.utils::removeColumns(result, "archive_file")
}

# isOnCran ---------------------------------------------------------------------
isOnCran <- function(name)
{
  nrow(currentCranVersion(name)) > 0L
}
