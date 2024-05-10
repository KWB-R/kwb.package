# getDependencyData ------------------------------------------------------------

#' Get Package Dependency Data from Package Database
#' 
#' @param db package data base as e.g. returned by
#'   \code{\link{getCranPackageDatabase}}
#' @param fields types of dependencies to be considered. Default:
#'   \code{c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")} 
#' @param dbg logical indicating whether or not to show debug messages
#' @export
getDependencyData <- function(
    db, 
    fields = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances"),
    dbg = FALSE
)
{
  stopifnot(`db must be a matrix or a data frame` = length(dim(db)) == 2L)
  
  db <- as.data.frame(db)
  packages <- selectColumns(db, "Package")
  
  lapply(intersect(fields, names(db)), function(field) {
    catAndRun(
      sprintf("Analysing %s field", field),
      dbg = dbg,
      packages %>% 
        toDependencyData(selectColumns(db, field)) %>% 
        cbind(type = tolower(field))
    )
  }) %>% 
    rbindAll() %>% 
    fullySorted() %>% 
    moveColumnsToFront(c("package", "type"))
}

# toDependencyData -------------------------------------------------------------
toDependencyData <- function(packages, strings)
{
  stopifnot(length(packages) == length(strings))
  
  # Split dependency strings at comma (after removing new line or space)
  parts <- strsplit(gsub("\n|\\s+", "", strings), ",")
  
  data.frame(
    package = rep(packages, lengths(parts)),
    splitDependency(unlist(parts))
  )
}

# splitDependency --------------------------------------------------------------
splitDependency <- function(x)
{
  stopifnot(`No spaces allowed!` = !any(grepl("\\s", x)))
  
  pattern <- "^([^(]+)(\\(([<>=]+)(.*)\\))?"
  matches <- is.na(x) | grepl(pattern, x)
  
  if (any(!matches)) {
    stopFormatted(
      "Non-matching dependency string: '%s'", x[!matches][1L]
    )
  }
  
  cbind(
    dep_full = x, 
    extractSubstring(pattern, x, index = c(
      dep_name = 1L, 
      dep_version_op = 3L, 
      dep_version_no = 4L
    ))
  )
}
