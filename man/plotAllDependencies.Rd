% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotAllDependencies.R
\name{plotAllDependencies}
\alias{plotAllDependencies}
\title{Plot all Package Dependencies}
\usage{
plotAllDependencies(dependencies, r = 1.5, for.each = TRUE, ...)
}
\arguments{
\item{dependencies}{list of package dependencies as returned by 
\code{\link{packageDependencies}}}

\item{r}{radius of the unit circle in which to arrange the package names.
Passed to \code{\link{plotNodes}}}

\item{for.each}{if \code{TRUE} (default) not only an overview plot showing
all dependencies but also one plot per package of which dependency
information are contained in \code{dependencies} is created.}

\item{\dots}{arguments passed to \code{\link{plotDependencies}}}
}
\description{
Plot all Package Dependencies
}
\examples{
kwb.packages <- installedKwbPackages()
  
# Plot all (direct and indirect) dependencies of installed kwb packages
plotAllDependencies(packageDependencies(kwb.packages))
  
# Plot only direct dependencies
plotAllDependencies(packageDependencies(kwb.packages, recursive = FALSE))

}
