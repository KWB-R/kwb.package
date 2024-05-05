# loadDescriptionFromArchiveUrl ------------------------------------------------

#' @noRd
#' @noMd
#' @keywords internal 
#' @importFrom utils download.file
loadDescriptionFromArchiveUrl <- function(
    url, 
    targetDir = tempdir(), 
    quiet = TRUE
)
{
  #url <- "https://cran.r-project.org/src/contrib/Archive/ggplot2/ggplot2_2.1.0.tar.gz"
  tarGzExtension <- ".tar.gz"
  stopifnot(endsWith(url, tarGzExtension))
  
  # Download .tar.gz file
  tarball <- downloadFile(url, targetDir = tempdir())
  on.exit(unlink(tarball))
  
  # Extract DESCRIPTION from downloaded .tar.gz file
  # see remotes:::load_pkg_description
  descriptionFile <- remotes_untar_description(tarball, dir = tempdir())
  on.exit(unlink(dirname(descriptionFile), recursive = TRUE), add = TRUE)
  
  # Copy the DESCRIPTION file with a unique name to the target directory
  targetFile <- copyFile(
    from = descriptionFile, 
    to = file.path(targetDir, paste0(
      "DESCRIPTION_", 
      gsub(tarGzExtension, ".txt", basename(tarball), fixed = TRUE)
    ))
  )
  
  readDescriptionAddingPath(targetFile)
}
