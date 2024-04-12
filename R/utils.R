# readLinesFromUrl -------------------------------------------------------------
readLinesFromUrl <- function(url, silent = TRUE)
{
  result <- try(suppressWarnings(readLines(url)), silent = silent)
  
  if (inherits(result, "try-error")) {
    return(NULL)
  }
  
  result
}

# stop_ ------------------------------------------------------------------------
stop_ <- function(...)
{
  stop(..., call. = FALSE)
}

