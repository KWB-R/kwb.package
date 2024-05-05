# parsePackageDeps -------------------------------------------------------------

#' @importFrom remotes standardise_dep
parsePackageDeps <- function(description, dependencies = NA) 
{
  types <- tolower(remotes::standardise_dep(dependencies))
  
  description[intersect(names(description), types)] %>% 
    lapply(remotes_parse_deps) %>% 
    rbindAll(nameColumn = "type")
}
