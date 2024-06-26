#' parsePackageDeps ------------------------------------------------------------
#' @noMd
#' @noRd
#' @keywords internal
parsePackageDeps <- function(pkg, dependencies = NA) 
{
  deps <- tolower(remotes::standardise_dep(dependencies))
  parsed <- lapply(pkg[intersect(deps, names(pkg))], remotes_parse_deps)
  rbindAll(parsed, nameColumn = "type")
}
