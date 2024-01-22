# detachAllNonSystemPackages ---------------------------------------------------

#' Detach all Non-System Packages
#' 
#' @export
#' 
detachAllNonSystemPackages <- function()
{
  attachedNames <- grep("^package:", search(), value = TRUE)
  
  names <- setdiff(attachedNames, packageString(systemPackages()))
  
  for (name in names) {
    
    if (name %in% search()) {
      
      cat("Detaching ", name, "... ")
      detach(name, unload = TRUE, force = FALSE, character.only = TRUE)  
      cat("ok.\n")
      
    } else {
      
      cat(name, "already detached.\n")
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
#'   acutally to be detached, e.g. use pattern = "^kwb\\." to only detach kwb
#'   packages. Default: ".*" (matching all package names)
#' @param dbg if \code{TRUE}, debug messages are shown
#' 
#' @export
#' 
detachRecursively <- function(package, pattern = ".*", dbg = FALSE)
{
  sortedPackages <- sortedDependencies(package, pattern = pattern, dbg = dbg)
  
  nonSystemPackages <- setdiff(sortedPackages, systemPackages())
                          
  namesToDetach <- intersect(search(), packageString(nonSystemPackages))
  
  cat("*** object names to detach:\n")
  print(namesToDetach)
  
  for (objectName in namesToDetach) {
    
    cat("Detaching", objectName, "...\n")
    detach(objectName, unload = TRUE, character.only = TRUE)      
  }  
}

# sortedDependencies -----------------------------------------------------------

#' Sorted Package Dependencies
#' 
#' Names of depending packages in the order of their dependency
#' 
#' @param package name of package of which dependencies are to be found
#' @param dbg if \code{TRUE}, debug messages are shown
#' 
#' @return vector of package names. The first element is the package itself,
#'   followed by the names of depending packages. You should be able to detach
#'   the packages in this order without any "package ... is required by ..."
#'   error
#'   
sortedDependencies <- function(package, dbg = FALSE)
{
  dependingOn <- packageDependencies(package, recursive = TRUE)[[1]]
  
  dependencies <- packageDependencies(dependingOn, recursive = FALSE)  
  
  allLeaves <- character()
  
  while (length(dependencies) > 0) {
    
    nDependencies <- lengths(dependencies)
    
    printAndWaitIf(dbg, list(
      dependencies = dependencies,
      nDependencies = nDependencies
    ))
      
    # Extract the "leaves" from the dependencies
    leaves <- names(dependencies)[nDependencies == 0]
    dependencies <- dependencies[nDependencies > 0]
    
    dependencies <- sapply(
      dependencies, 
      function(x) {setdiff(x, leaves)},
      simplify = FALSE
    )
    
    # Add the leaves at the beginning of the vector of all leaves
    allLeaves <- c(leaves, allLeaves)
  }

  c(package, allLeaves)
}

# packageDependencies ----------------------------------------------------------

#' Package Dependencies
#' 
#' @param packages passed to \code{\link[tools]{package_dependencies}}
#' @param recursive passed to \code{\link[tools]{package_dependencies}}
#' @param reverse passed to \code{\link[tools]{package_dependencies}}
#' 
#' @export
#'  
packageDependencies <- function(
  packages = NULL, recursive = TRUE, reverse = FALSE
)
{
  tools::package_dependencies(
    packages, 
    db = utils::installed.packages(), 
    recursive = recursive, 
    reverse = reverse
  )
}

# printAndWaitIf ---------------------------------------------------------------

printAndWaitIf <- function(dbg, variables) 
{
  if (dbg) {
    
    for (variable in names(variables)) {
      cat("\n", variable, ":\n")
      print(variables[[variable]])      
    }
    
    readline(prompt = "Press Return to continue...")
  }
}

# systemPackages --------------------------------------------------------------

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
  
  stop("set_number must be one of 1, 2.")
}
