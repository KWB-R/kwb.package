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
#'   actually to be detached, e.g. use pattern = "^kwb\\." to only detach kwb
#'   packages. Default: ".*" (matching all package names)
#' @param dbg if \code{TRUE}, debug messages are shown
#' 
#' @export
#' 
detachRecursively <- function(package, pattern = ".*", dbg = FALSE)
{
  packages <- sortedDependencies(package, dbg = dbg)
  packages <- grep(pattern, packages, value = TRUE)
  
  nonSystemPackages <- setdiff(packages, systemPackages())
                          
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

# packageDependenciesByType ----------------------------------------------------

#' Package Dependencies by Type
#' 
#' @inheritParams tools::package_dependencies
#' @export
packageDependenciesByType <- function(
    packages = NULL, 
    db = utils::installed.packages(),
    which = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")[1:3],
    recursive = TRUE, 
    reverse = FALSE, 
    verbose = FALSE
)
{
  #kwb.utils::assignPackageObjects("kwb.package");`%>%` <- magrittr::`%>%`
  #packages <- c("kwb.utils", "kwb.plot")
  
  n_packages <- length(packages)
  
  if (n_packages == 0L) {
    return(NULL)
  }
  
  if (n_packages > 1L) {
    
    # Call this function recursively for each package
    return(
      lapply(
        X = stats::setNames(nm = packages), 
        FUN = packageDependenciesByType, 
        db = db,
        which = which,
        recursive = recursive, 
        reverse = reverse, 
        verbose = verbose
      ) %>% 
        do.call(what = rbind) %>% 
        resetRowNames()
    )
  }
  
  stopifnot(n_packages == 1L)
  
  which %>% 
    lapply(function(which.one) {
      #which.one <- "Imports"
      #print(which.one)
      dependencies <- tools::package_dependencies(
        packages, 
        db = db,
        which = which.one,
        recursive = recursive,
        reverse = reverse,
        verbose = verbose
      )[[1L]]
      if (length(dependencies)) {
        data.frame(
          package = packages,
          type = rep(which.one, length(dependencies))
        ) %>% 
          cbind(
            getPackageLicences(dependencies) %>% 
              renameColumns(list(package = "dependency"))
          )
      } # else NULL
    }) %>% 
    stats::setNames(which) %>% 
    excludeNULL(dbg = FALSE) %>% 
    do.call(what = rbind.data.frame) %>% 
    resetRowNames()
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
