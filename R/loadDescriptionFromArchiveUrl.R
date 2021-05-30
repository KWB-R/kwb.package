# loadDescriptionFromArchiveUrl ------------------------------------------------
loadDescriptionFromArchiveUrl <- function(url, destdir = tempdir())
{
  stopifnot(endsWith(url, ".tar.gz"))

  filename_tar <- basename(url)
  filename_desc <- paste0(filename_tar, "_DESCRIPTION")
  path_desc <- file.path(tempdir(), filename_desc)

  if (file.exists(path_desc)) {
    
    message("Reading from existing ", filename_desc)
    
  } else {

    # Download .tar.gz file
    destfile <- file.path(destdir, filename_tar)
    utils::download.file(url, destfile)
    on.exit(unlink(destfile))

    # Extract DESCRIPTION from downloaded .tar.gz file
    # see remotes:::load_pkg_description
    dir <- tempfile()
    path_desc_tmp <- remotes:::untar_description(destfile, dir = dir)
    on.exit(unlink(dir, recursive = TRUE), add = TRUE)
    
    # Keep only the DESCRIPTION in a .tar.gz.DESCRIPTION file
    file.copy(path_desc_tmp, path_desc)
  }

  # see remotes:::load_pkg_description
  desc <- remotes:::read_dcf(path_desc)
  names(desc) <- tolower(names(desc))
  desc$path <- path_desc
  desc
}
