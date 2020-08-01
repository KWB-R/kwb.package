# packageInDestdir -------------------------------------------------------------
packageInDestdir <- function(package, destdir, verbose = TRUE)
{
  files <- dirPackageZips(package, kwb.utils::safePath(destdir))
  
  file_exists <- length(files) > 0L
  
  if (verbose && file_exists) {
    message("Skipping already downloaded package '", package, "'")
  }
  
  structure(file_exists, path = if (file_exists) kwb.utils::lastElement(files))
}

# dirPackageZips ---------------------------------------------------------------
dirPackageZips <- function(package, path) 
{
  dir(path, paste0("^", package, "_"), full.names = TRUE)
}
