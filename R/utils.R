# anglesToPoints ---------------------------------------------------------------

#' Angle in Unit Circle to Point Coordinate
#' 
#' Convert angles in a unit circle to point coordinates (x, y)
#' 
#' @param angles.grad vector of angles in degree
#' 
#' @return matrix with columns \emph{x} and \emph{y} containing the coordinates
#'   of points corresponding to the given angles in a unit circle
#'
#' @export
#' 
#' @examples 
#' plot(anglesToPoints(equidistantAngles(n = 10)), type = "b")
#'   
anglesToPoints <- function(angles.grad)
{
  polar_to_xy(gradToRad(angles.grad))
}

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

# equidistantAngles ------------------------------------------------------------

#' Equidistantly Distributed Angles in Degrees Between 0 and 360
#' 
#' @param n number of angles to be returned
#' @param from start angle in degrees. Default: 0
#' 
#' @export
#' 
#' @examples 
#' x <- equidistantAngles(90)
#' 
#' plot(x, sin(gradToRad(x)), xlab = "angle in degree", ylab = "sin(angle)")
#' 
equidistantAngles <- function(n, from = 0)
{
  seq(from, by = 360/n, length.out = n)
}

# excludeNull ------------------------------------------------------------------
#' @importFrom kwb.utils excludeNULL
excludeNull <- kwb.utils::excludeNULL

# extractSubstring -------------------------------------------------------------
#' @importFrom kwb.utils extractSubstring
extractSubstring <- kwb.utils::extractSubstring

# fullySorted ------------------------------------------------------------------
#' @importFrom kwb.utils fullySorted
fullySorted <- kwb.utils::fullySorted

# getAttribute -----------------------------------------------------------------
#' @importFrom kwb.utils getAttribute
getAttribute <- kwb.utils::getAttribute

# getHomedir -------------------------------------------------------------------
#' @importFrom kwb.utils get_homedir
getHomedir <- kwb.utils::get_homedir

# gradToRad --------------------------------------------------------------------

#' Angle in Degree to Angle in rad
#' 
#' @param grad vector of angles in degrees
#'
#' @export
#'  
#' @examples 
#' gradToRad(c(0, 90, 180, 270, 360)) / pi
#' 
gradToRad <- function(grad)
{
  grad/180 * pi
}

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
runInDirectory <- kwb.utils::runInDirectory

# safePath ---------------------------------------------------------------------
#' @importFrom kwb.utils safePath
safePath <- kwb.utils::safePath

# safeRowBind ------------------------------------------------------------------
#' @importFrom kwb.utils safeRowBind
safeRowBind <- kwb.utils::safeRowBind

# selectColumns ----------------------------------------------------------------
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

