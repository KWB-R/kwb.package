% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/detach.R
\name{detachRecursively}
\alias{detachRecursively}
\title{Detach Packages Recursively}
\usage{
detachRecursively(package, pattern = ".*", dbg = FALSE)
}
\arguments{
\item{package}{name of package to be detached}

\item{pattern}{pattern matching the names of depending packages that are
actually to be detached, e.g. use pattern = "^kwb\\." to only detach kwb
packages. Default: ".*" (matching all package names)}

\item{dbg}{if \code{TRUE}, debug messages are shown}
}
\description{
Detach a package and all the depending packages
}
