# getPackageLicences -----------------------------------------------------------

#' Which Licences are Specified for the Packages?
#' 
#' @param packages names of (installed) packages
#' @param db optional. Package database, similar to what is returned by
#'   \code{\link[utils]{installed.packages}}. Default:
#'   \code{as.data.frame(installed.packages())}
#' @return data frame
#' @importFrom utils installed.packages
#' @export
getPackageLicences <- function(
    packages, 
    db = as.data.frame(utils::installed.packages())
)
{
  #kwb.utils::assignPackageObjects("kwb.package");stop.on.error = FALSE
  #`%>%` <- magrittr::`%>%`
  #db <- kwb.utils:::get_cached("package_db")
  #packages <- db$Package

  colnames(db) <- tolower(colnames(db))
  licence_fields <- intersect(colnames(db), c("licence", "license"))
  stopifnot(length(licence_fields) == 1L)

  backbone <- data.frame(
    package = packages, 
    stringsAsFactors = FALSE
  )
  
  result <- backbone %>% 
    merge(
      y = db[, c("package", licence_fields)], 
      by = "package", 
      all.x = TRUE
    ) %>% 
    renameColumns(list(license = "licence")) %>% 
    orderBy("package")
  
  result[["licence"]] <- defaultIfNa(result[["licence"]], "<not_found>")
  
  result
}
