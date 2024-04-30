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
    lapply(function(type) {
      #type <- "Imports"
      #print(type)
      dependencies <- tools::package_dependencies(
        packages, 
        db = db,
        which = type,
        recursive = recursive,
        reverse = reverse,
        verbose = verbose
      )[[1L]]
      if (length(dependencies)) {
        data.frame(
          package = packages,
          type = rep(type, length(dependencies))
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
