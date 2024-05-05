# loadDescriptionFromArchiveUrl ------------------------------------------------

#' @noRd
#' @noMd
#' @keywords internal 
#' @importFrom utils download.file
loadDescriptionFromArchiveUrl <- function(url, path = tempfile())
{
  untarDescriptionFromUrl(url, path)

  # see remotes:::load_pkg_description
  desc <- remotes_read_dcf(path)
  names(desc) <- tolower(names(desc))
  desc$path <- path
  
  desc
}

# untarDescriptionFromUrl ------------------------------------------------------
untarDescriptionFromUrl <- function(url, target = NULL, destdir = tempdir())
{
  stopifnot(endsWith(url, ".tar.gz"))
  
  tarname <- basename(url)

  # Download .tar.gz file
  destfile <- file.path(destdir, tarname)
  utils::download.file(url, destfile)
  on.exit(unlink(destfile))
  
  # Extract DESCRIPTION from downloaded .tar.gz file
  # see remotes:::load_pkg_description
  dir <- tempfile()
  target_tmp <- remotes_untar_description(destfile, dir = dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  
  # Keep only the DESCRIPTION in a .tar.gz.DESCRIPTION file
  target <- defaultIfNull(target, pathDescription(tarname, destdir))

  file.copy(target_tmp, target)
  
  target
}

# pathDescription --------------------------------------------------------------
pathDescription <- function(name, version, tarname, destdir = tempdir())
{
  #name <- gsub("\\.tar\\.gz$", "", tarname)
  #file.path(destdir, sprintf("DESCRIPTION_%s.txt", name))
  #path <- pathDescription(tarname)
  file.path(destdir, getUrl(
    "cached_desc", package = name, version = version
  ))
  
}
