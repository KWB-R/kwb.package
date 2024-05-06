# detachAllNonSystemPackages ---------------------------------------------------

#' Detach all Non-System Packages
#' 
#' @param dbg logical indicating whether or not to show debug messages
#' @export
#' 
detachAllNonSystemPackages <- function(dbg = TRUE)
{
  attached <- grep("^package:", search(), value = TRUE)
  
  names <- setdiff(attached, packageString(systemPackages()))
  
  for (name in names) {
    
    if (name %in% search()) {
      
      catAndRun(
        paste("Detaching", name), 
        detach(name, unload = TRUE, force = FALSE, character.only = TRUE),
        dbg = dbg
      )
      
    } else {
      
      catIf(dbg, name, "already detached.\n")
    }
  }
}

# detachRecursively ------------------------------------------------------------

#' Detach Packages Recursively
#' 
#' Detach a package and all the depending packages
#' 
#' @param package name of package to be detached
#' @param pattern pattern matching the names of depending packages that are
#'   actually to be detached, e.g. use pattern = "^kwb\\." to only detach kwb
#'   packages. Default: ".*" (matching all package names)
#' @param dbg if \code{TRUE}, debug messages are shown
#' 
#' @export
#' 
detachRecursively <- function(package, pattern = ".*", dbg = FALSE)
{
  #kwb.utils::assignPackageObjects("kwb.package");dbg=TRUE;pattern = ".*"
  namesToDetach <- package %>% 
    sortedDependencies() %>% 
    grep(pattern = pattern, value = TRUE) %>% 
    setdiff(systemPackages()) %>% 
    packageString() %>% 
    intersect(search())
  
  kwb.utils::printIf(dbg, namesToDetach, caption = "*** object names to detach")
  
  for (name in namesToDetach) {
    catAndRun(
      paste("Detaching", name),
      detach(name, unload = TRUE, character.only = TRUE),
      dbg = dbg
    )
  }  
}

# sortedDependencies -----------------------------------------------------------

#' Sorted Package Dependencies
#' 
#' Names of depending packages in the order of their dependency
#' 
#' @param package name of package(s) of which dependencies are to be found
#' @param dbg if \code{TRUE}, debug messages are shown and the user is asked
#'   to press Enter each time the body of the main loop is passed!
#' @return vector of package names. The first element is the package itself,
#'   followed by the names of depending packages. You should be able to detach
#'   the packages in this order without any "package ... is required by ..."
#'   error
#' @export
sortedDependencies <- function(package, dbg = FALSE)
{
  fullDependencies <- packageDependencies(package, recursive = TRUE)
  
  # Put all unique package names from the dependency tree into a vector
  packages <- unique(unname(unlist(fullDependencies)))
  
  # Get sorted vector of unique package names (including those in package)
  packages <- sort(unique(c(packages, package)))

  #packages <- fullDependencies[[1]]

  # Get the "direct" (non-recursive) dependencies for each package
  dependencies <- packageDependencies(packages, recursive = FALSE)  
  
  allLeaves <- character()
  
  while (length(dependencies)) {
    
    nDependencies <- lengths(dependencies)
    
    printAndWaitIf(dbg, list(
      dependencies = dependencies,
      nDependencies = nDependencies
    ))

    # Which dependency is a "leaf" (i.e. does not have further dependencies)?
    isLeaf <- nDependencies == 0L
    
    # Extract the "leaves" from the dependencies
    leaves <- names(dependencies)[isLeaf]

    # Remove the names of the leaf packages from the dependency tree
    dependencies <- sapply(
      dependencies[!isLeaf], 
      FUN = setdiff,
      y = leaves,
      simplify = FALSE
    )
    
    # Add the leaves at the beginning of the vector of all leaves
    allLeaves <- c(leaves, allLeaves)
  }

  allLeaves
}

# packageDependencies ----------------------------------------------------------

#' Package Dependencies
#'
#' This is just a wrapper around \code{\link[tools]{package_dependencies}} with
#' some defaults defined.
#' 
#' @inheritParams tools::package_dependencies
#' @export
packageDependencies <- function(
  packages = NULL, 
  db = utils::installed.packages(),
  which = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")[1:3],
  recursive = TRUE, 
  reverse = FALSE, 
  verbose = FALSE
)
{
  tools::package_dependencies(
    packages, 
    db = db, 
    which = which,
    recursive = recursive, 
    reverse = reverse,
    verbose = verbose
  )
}

# printAndWaitIf ---------------------------------------------------------------
printAndWaitIf <- function(dbg, variables) 
{
  if (!dbg) {
    return()
  }
  
  for (variable in names(variables)) {
    cat("\n", variable, ":\n")
    print(variables[[variable]])      
  }
  
  readline(prompt = "Press Return to continue...")
}

# systemPackages ---------------------------------------------------------------

#' Names of Base R Packages
#' 
#' @param set_number integer number specifying a set of packages: 1 or 2.
#' @return vector of character representing package names
#' @export
#' 
systemPackages <- function(set_number = 1L)
{
  common <- c("stats", "graphics", "grDevices", "utils", "methods")
    
  if (set_number == 1L) {
    return(c(common, "datasets", "base"))
  } 
  
  if (set_number == 2L) {
    return(c(common, "grid", "splines", "tools"))
  }
  
  cleanStop("set_number must be one of 1, 2.")
}
