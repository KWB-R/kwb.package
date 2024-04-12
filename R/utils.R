# catAndRun --------------------------------------------------------------------

#' @importFrom kwb.utils catAndRun  
catAndRun <- kwb.utils::catAndRun

# catIf ------------------------------------------------------------------------

#' @importFrom kwb.utils catIf
catIf <- kwb.utils::catIf

# cleanStop --------------------------------------------------------------------
cleanStop <- function(...)
{
  stop(..., call. = FALSE)
}

# createDirectory --------------------------------------------------------------
#' @importFrom kwb.utils createDirectory
createDirectory <- kwb.utils::createDirectory

# defaultIfNa ------------------------------------------------------------------
#' @importFrom kwb.utils defaultIfNA
defaultIfNa <- kwb.utils::defaultIfNA

# defaultIfNull ----------------------------------------------------------------
#' @importFrom kwb.utils defaultIfNULL
defaultIfNull <- kwb.utils::defaultIfNULL

# excludeNull ------------------------------------------------------------------
#' @importFrom kwb.utils excludeNULL
excludeNull <- kwb.utils::excludeNULL

# extractSubstring -------------------------------------------------------------
#' @importFrom kwb.utils extractSubstring
extractSubstring <- kwb.utils::extractSubstring

# getAttribute -----------------------------------------------------------------
#' @importFrom kwb.utils getAttribute
getAttribute <- kwb.utils::getAttribute

# getHomedir -------------------------------------------------------------------
#' @importFrom kwb.utils get_homedir
getHomedir <- kwb.utils::get_homedir

# lastElement ------------------------------------------------------------------
#' @importFrom kwb.utils lastElement
lastElement <- kwb.utils::lastElement

# moveColumnsToFront -----------------------------------------------------------
moveColumnsToFront <- kwb.utils::moveColumnsToFront

# noFactorDataFrame ------------------------------------------------------------
#' @importFrom kwb.utils noFactorDataFrame
noFactorDataFrame <- kwb.utils::noFactorDataFrame

# noSuchElements ---------------------------------------------------------------
#' @importFrom kwb.utils noSuchElements
noSuchElements <- kwb.utils::noSuchElements

# orderBy ----------------------------------------------------------------------
#' @importFrom kwb.utils orderBy
orderBy <- kwb.utils::orderBy

# rbindAll ---------------------------------------------------------------------
#' @importFrom kwb.utils rbindAll
rbindAll <- kwb.utils::rbindAll

# readLinesFromUrl -------------------------------------------------------------
readLinesFromUrl <- function(url, silent = TRUE)
{
  result <- try(suppressWarnings(readLines(url)), silent = silent)
  
  if (inherits(result, "try-error")) {
    return(NULL)
  }
  
  result
}

# removeColumns ----------------------------------------------------------------
#' @importFrom kwb.utils removeColumns
removeColumns <- kwb.utils::removeColumns

# renameColumns ----------------------------------------------------------------
#' @importFrom kwb.utils renameColumns
renameColumns <- kwb.utils::renameColumns
# resetRowNames ----------------------------------------------------------------
#' @importFrom kwb.utils resetRowNames
resetRowNames <- kwb.utils::resetRowNames

# resolve ----------------------------------------------------------------------
#' @importFrom kwb.utils resolve
resolve <- kwb.utils::resolve

# restoreAttributes ------------------------------------------------------------
#' @importFrom kwb.utils hsRestoreAttributes
restoreAttributes <- kwb.utils::hsRestoreAttributes

# runInDirectory ---------------------------------------------------------------
#' @importFrom kwb.utils runInDirectory
kwb.utils::runInDirectory

# safePath ---------------------------------------------------------------------
#' @importFrom kwb.utils safePath
kwb.utils::safePath

# safeRowBind ------------------------------------------------------------------
#' @importFrom kwb.utils safeRowBind
safeRowBind <- kwb.utils::safeRowBind

# selectColumns ---------------------------------------------------------------
#' @importFrom kwb.utils selectColumns
selectColumns <- kwb.utils::selectColumns


# selectElements ---------------------------------------------------------------
#' @importFrom kwb.utils selectElements
selectElements <- kwb.utils::selectElements

# stopFormatted ----------------------------------------------------------------
#' @importFrom kwb.utils stopFormatted
stopFormatted <- kwb.utils::stopFormatted

# removeColumns ----------------------------------------------------------------
#' @importFrom kwb.utils removeColumns
removeColumns <- kwb.utils::removeColumns
