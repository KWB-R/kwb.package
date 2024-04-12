# downloadPackagesFromSnapshot -------------------------------------------------

#' Download Package Archive from Microsoft R Archive Network
#' 
#' @param packages names of packages (vector of character)
#' @param snapshot_date date of snapshot of CRAN package versions, as a 
#'   string in yyyy-mm-dd format
#' @param destdir path to download folder
#' @param type one of \code{c("source", "win.binary")}
#' @return paths to the downloaded files (vector of character)
#' @export
#' @importFrom utils download.packages
downloadPackagesFromSnapshot <- function(
  packages, snapshot_date, destdir = NULL, type = c("source", "win.binary")[1L]
)
{
  stopifnot(is.character(packages))
  
  type <- match.arg(type, c("source", "win.binary"))
  
  n <- length(packages)
  
  if (n == 0L) {
    return(character(0))
  }
  
  repos <- getPath("mran_snapshot", date = snapshot_date)

  if (is.null(destdir)) {
    
    success <- dir.create(destdir <- tempfile("snapshot_"))
    
  } else {
    
    stopifnot(file.exists(destdir))
  }
  
  for (i in seq_len(n)) {
    
    package <- packages[i]
    
    if (! packageInDestdir(package, destdir)) {

      catAndRun(
        sprintf("Downloading %s package %d/%d: %s", type, i, n, package),
        utils::download.packages(
          package, 
          destdir = destdir, 
          repos = repos, 
          type = type
        ),
        newLine = 3L
      )
    }
  }

  dir(destdir, full.names = TRUE)
}
