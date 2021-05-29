# parsePackageDeps -------------------------------------------------------------
parsePackageDeps <- function(pkg, dependencies = NA) 
{
  deps <- tolower(remotes::standardise_dep(dependencies))
  parsed <- lapply(pkg[intersect(deps, names(pkg))], remotes:::parse_deps)
  kwb.utils::rbindAll(parsed, nameColumn = "type")
}
