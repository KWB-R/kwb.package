# getPackageLicences -----------------------------------------------------------

#' Which Licences are Specified for the Packages?
#' 
#' @param packages names of (installed) packages
#' @return data frame
#' @importFrom kwb.utils moveColumnsToFront rbindAll
#' @export
getPackageLicences <- function(packages, stop.on.error = FALSE)
{
  lapply(packages, function(package) {
    description <- readDescription(package, stop.on.error = stop.on.error)
    if (is.null(description)) {
      data.frame(licence = "<not_found>")
    } else {
      columns <- intersect(colnames(description), c("licence", "license"))
      description[, columns, drop = FALSE] %>%
        as.data.frame() %>% 
        renameColumns(list(license = "licence"))
    } # else NULL
  }) %>%
    stats::setNames(packages) %>%
    rbindAll(nameColumn = "package", namesAsFactor = FALSE) %>% 
    moveColumnsToFront("package")
}
