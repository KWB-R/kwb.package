# getPackageLicences -----------------------------------------------------------

#' Which Licences are Specified for the Packages?
#' 
#' @param packages names of (installed) packages
#' @param db optional. Package database, similar to what is returned by
#'   \code{\link[utils]{installed.packages}}. Default:
#'   \code{installed.packages()}
#' @return data frame
#' @importFrom utils installed.packages
#' @export
getPackageLicences <- function(
    packages, 
    db = utils::installed.packages()
)
{
  #kwb.utils::assignPackageObjects("kwb.package");stop.on.error = FALSE
  #`%>%` <- magrittr::`%>%`
  #db <- kwb.utils:::get_cached("package_db")
  #packages <- db$Package

  db <- as.data.frame(db)
  names(db) <- gsub("license", "licence", tolower(names(db)))

  licence_fields <- grep("^licence", names(db), value = TRUE)
  stopifnot(length(licence_fields) > 0L)

  backbone <- data.frame(
    package = packages, 
    stringsAsFactors = FALSE
  )
  
  result <- backbone %>% 
    merge(
      y = selectColumns(db, c("package", "version", licence_fields)), 
      by = "package", 
      all.x = TRUE
    ) %>% 
    renameColumns(list(license = "licence")) %>% 
    orderBy("package")
  
  result[["licence"]] <- defaultIfNa(result[["licence"]], "<not_found>")
  
  result
}
