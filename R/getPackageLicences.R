# getPackageLicences -----------------------------------------------------------

#' Which Licences are Specified for the Packages?
#' 
#' @param packages names of (installed) packages
#' @return data frame
#' @export
getPackageLicences <- function(packages)
{
  lapply(packages, function(package) {
    description <- readDescription(package)
    fields <- c("licence", "license")
    columns <- intersect(colnames(description), fields)
    as.data.frame(description[, columns, drop = FALSE])
  }) %>%
    stats::setNames(packages) %>%
    rbindAll(nameColumn = "package") %>% 
    moveColumnsToFront("package")
}

# readDescription --------------------------------------------------------------
readDescription <- function(package)
{
  description <- "DESCRIPTION" %>% 
    system.file(package = package, mustWork = TRUE) %>% 
    read.dcf()
  
  colnames(description) <- tolower(colnames(description))
  description
}
