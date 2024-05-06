# allDeps ----------------------------------------------------------------------
allDeps <- function(
  name, 
  version = NA, 
  depth = 1L, 
  max_depth = 9L,
  cache = list()
)
{
  #kwb.utils::assignPackageObjects("kwb.package");name="abc";version=NA;depth=1L;max_depth=9L;cache=list()
  
  description <- loadDescriptionFromWeb(name, version)
  versionInDescription <- selectElements(description, "version")
  
  stopifnot(is.na(version) || identical(version, versionInDescription))
  
  deps <- parsePackageDeps(description)

  if (inherits(deps, "try-error") || nrow(deps) == 0L) {
    return(NULL)
  }
  
  message("depth: ", depth)
  
  deps$depth <- depth
  deps$namever <- paste(name, version, sep = ":")
  
  if (depth == max_depth) {
    message(sprintf("maximum depth (%s) reached.", max_depth))
    return(deps)
  }
  
  child_deps <- seq_len(nrow(deps)) %>% 
    lapply(function(i) {
      allDeps(deps$name[i], deps$version[i], depth + 1L, max_depth)
    }) %>% 
    excludeNull(dbg = FALSE)
  
  if (length(child_deps) > 0L) {
    deps <- rbind(deps, do.call(rbind, child_deps))
  }
  
  deps
}
