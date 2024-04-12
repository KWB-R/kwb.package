# currentCranVersion -----------------------------------------------------------

#' @noMd
#' @noRd
#' @keywords internal
currentCranVersion <- function(name)
{
  src <- readLinesFromUrl(getUrl("cran_package", package = name))
  
  was_removed_pattern <- "was removed from the CRAN repository"
  
  if (is.null(src) || any(grepl(was_removed_pattern, src))) {
    return(noFactorDataFrame(
      package = character(0L),
      version = character(0L),
      date = as.Date(character(0L)),
      date_type = character(0L)
    ))
  }
  
  extract <- function(x) {
    gsub("<td>|</td>", "", src[grep(sprintf("<td>%s:</td>", x), src) + 1L])
  }
  
  noFactorDataFrame(
    package = name,
    version = extract("Version"),
    date = as.Date(extract("Published")),
    date_type = "published"
  )
}

# getCranPackageDatabase -------------------------------------------------------

#' Get Matrix with Information on All CRAN Packages
#' 
#' @export
getCranPackageDatabase <- function()
{
  readRDS(file(getPath("cran_packages", package = "packages.rds")))
}

# isOnCran ---------------------------------------------------------------------
isOnCran <- function(name)
{
  nrow(currentCranVersion(name)) > 0L
}
