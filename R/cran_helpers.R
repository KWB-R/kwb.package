# currentCranVersion -----------------------------------------------------------

#' @noMd
#' @noRd
#' @keywords internal
currentCranVersion <- function(name)
{
  text <- getUrl("cran_package", package = name) %>% 
    readLinesFromUrl()
  
  if (
    is.null(text) || 
    any(grepl("was removed from the CRAN repository", text))
  ) {
    return(noFactorDataFrame(
      package = character(0L),
      version = character(0L),
      date = as.Date(character(0L)),
      date_type = character(0L)
    ))
  }
  
  extract <- function(x) {
    text[grep(sprintf("<td>%s:</td>", x), text) + 1L] %>% 
    gsub(pattern = "<td>|</td>", replacement = "")
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
  url <- getUrl("cran_package", package = "packages.rds")
  
  system.time(db1 <- as.data.frame(readRDS(file(url))))
  
  #system.time(db2 <- tools::CRAN_package_db())
  #stopifnot(identical(db1, db2))

  db1
}

# isOnCran ---------------------------------------------------------------------
isOnCran <- function(name)
{
  nrow(currentCranVersion(name)) > 0L
}
