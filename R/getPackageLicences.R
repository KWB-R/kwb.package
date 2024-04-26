# getPackageLicences -----------------------------------------------------------

#' Which Licences are Specified for the Packages?
#' 
#' @param packages names of (installed) packages
#' @return data frame
#' @importFrom kwb.utils moveColumnsToFront rbindAll
#' @export
getPackageLicences <- function(packages)
{
  lapply(packages, function(package) {
    description <- readDescription(package)
    fields <- c("licence", "license")
    columns <- intersect(colnames(description), fields)
    as.data.frame(description[, columns, drop = FALSE]) %>%
      kwb.utils::renameColumns(list(
        license = "licence"
      ))
  }) %>%
    stats::setNames(packages) %>%
    kwb.utils::rbindAll(nameColumn = "package", namesAsFactor = FALSE) %>% 
    kwb.utils::moveColumnsToFront("package")
}
