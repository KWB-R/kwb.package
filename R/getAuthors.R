# getAuthors -------------------------------------------------------------------

#' Get Information on Package Authors
#' 
#' @param package name of (installed) package
#' @export
getAuthors <- function(package)
{
  description <- readDescription(package)
  columns <- colnames(description)
  columns <- grep("author", columns, value = TRUE, ignore.case = TRUE)
  description[, columns]
}
