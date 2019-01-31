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
#' @param pattern pattern matching the names of packages to be considered
#' @param dbg if \code{TRUE}, debug messages are shown
#' 
#' @return vector of package names. The first element is the package itself,
#'   followed by the names of depending packages. You should be able to detach
#'   the packages in this order without any "package ... is required by ..."
#'   error
#'   
sortedDependencies <- function(package, pattern = ".*", dbg = FALSE) 
{
  dependingOn <- grep(
    pattern, 
    packageDependencies(package, recursive = TRUE)[[1]], 
    value = TRUE
  )
  
  dependencies <- packageDependencies(dependingOn, recursive = FALSE)  
  
  allLeafs <- character()
  
  while (length(dependencies) > 0) {
    
    numberOfDependencies <- sapply(dependencies, FUN = function(x) {
      length(grep(pattern, x))
    })
    
    printAndWaitIf(dbg, list(
      dependencies = dependencies,
      numberOfDependencies = numberOfDependencies
    ))
      
    # Extract the "leafs" from the dependencies
    leafs <- names(dependencies)[numberOfDependencies == 0]
    dependencies <- dependencies[numberOfDependencies > 0]
    
    dependencies <- sapply(
      dependencies, 
      function(x) {setdiff(x, leafs)},
      simplify = FALSE
    )
    
    # Add the leafs at the beginning of the vector of all leafs
    allLeafs <- c(leafs, allLeafs)
  }

  c(package, allLeafs)
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

systemPackages <- function()
{
  c("stats", "graphics", "grDevices", "utils", "datasets", "methods", "base")  
}
