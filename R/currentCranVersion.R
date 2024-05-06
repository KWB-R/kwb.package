# currentCranVersion -----------------------------------------------------------
currentCranVersion <- function(name, v = 1L)
{
  if (v == 1L) {
    currentCranVersion_v1(name)
  } else if (v == 2L) {
    currentCranVersion_v2(name)
  } else {
    stopFormatted("v (version) must be one of 1 or 2.")
  }
}

# currentCranVersion_v1 --------------------------------------------------------
currentCranVersion_v1 <- function(name)
{
  text <- "cran_package" %>% 
    getUrl(package = name) %>% 
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
  
  tdOpen <- "<td>"
  tdClose <- "</td>"
  
  extract <- function(x) {
    patternFind <- paste0(tdOpen, x, ":", tdClose)
    patternReplace <- paste0(tdOpen, "|", tdClose)
    text[grep(patternFind, text) + 1L] %>% 
      gsub(pattern = patternReplace, replacement = "")
  }
  
  noFactorDataFrame(
    package = name,
    version = extract("Version"),
    date = as.Date(extract("Published")),
    date_type = "published"
  )
}

# currentCranVersion_v2 --------------------------------------------------------
currentCranVersion_v2 <- function(name)
{
  db <- getCranPackageDatabase()
  isPackage <- selectColumns(db, "Package") == name
  data <- db[isPackage, ]
  noFactorDataFrame(
    package = name,
    version = selectColumns(data, "Version"),
    date = as.Date(selectColumns(data, "Published")),
    date_type = "published"
  )
}

# getCranPackageDatabase -------------------------------------------------------

#' Get Matrix with Information on All CRAN Packages
#' 
#' @export
getCranPackageDatabase <- function()
{
  system.time(
    db_1 <- "cran_package" %>% 
      getUrl(package = "packages.rds") %>% 
      file() %>% 
      readRDS() %>% 
      asNoFactorDataFrame()
  )
  
  #system.time(db2 <- tools::CRAN_package_db())
  #stopifnot(identical(db1, db2))
  
  db_1
}
