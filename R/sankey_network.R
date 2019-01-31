# plotSankeyNetwork ------------------------------------------------------------

#' Plot Sankey Network
#' 
#' @param functionName name of function from which to start the network
#' @param where passed to \code{\link[mvbutils]{foodweb}}
#' @param \dots additional arguments passed to \code{\link[mvbutils]{foodweb}}
#' 
#' @export
#' 
plotSankeyNetwork <- function(functionName, where = 1, ...)
{
  relationInfo <- mvbutils::foodweb(
    where = where, plotting = FALSE, prune = functionName
    # , ancestors = FALSE
    , ...
  )

  relations <- relationInfo$funmat
  #relations <- includeExclude(relations, exclude = "^(init|demo|warn|validate)")

  network <- toLinksAndNodes(relationMatrixToSourceTarget(x = relations))

  networkD3::sankeyNetwork(
    network$links, network$nodes, Source = "source", Target = "target",
    Value = "value", NodeID = "name", NodeGroup = "name",
    nodeWidth = 10, nodePadding = 25,
    fontSize = 10
  )
}

# packageString ----------------------------------------------------------------

#' Package String
#' 
#' @param package Package name
#' 
#' @return \code{package}, preceded by \code{package:}
#' 
#' @export
#' 
packageString <- function(package)
{
  paste0("package:", package)
}

# exampleLinksAndNodes ---------------------------------------------------------

exampleLinksAndNodes <- function()
{
  list(
    Links = utils::read.table(
      sep = ",", header = TRUE, text = "
      Source,Target,Value,Group
      0,1,1,A
      0,1,2,B
      0,2,1,C
      0,3,1,C
      0,3,1,B
      0,3,1,A
      1,2,1,A
      "
    ),

    Nodes = utils::read.table(
      sep = ",", header = TRUE, text = "
      ID,Name
      0,main
      1,f1
      2,f2
      3,f3
      "
    )
  )
}

# toLinksAndNodes --------------------------------------------------------------

#' Convert Links to List of Links and Nodes
#' 
#' @param links list with elements \code{source} and \code{target}
#' @return list with elements \code{links} (input list \code{links} with new 
#'   elements \code{value}, \code{source}, \code{target}) and \code{nodes}
#'   (data frame with column \code{name})
#'
#' @export
#' 
#' @examples 
#' kwb.package:::toLinksAndNodes(list(
#'   source = c("s1", "s1"), target = c("t1", "t2")
#' ))
#' 
toLinksAndNodes <- function(links)
{
  selection <- kwb.utils::selectElements(links, "source")

  sources <- as.character(kwb.utils::defaultIfNA(selection, ""))
  targets <- as.character(kwb.utils::selectElements(links, "target"))

  nodes <- data.frame(name = unique(c(sources, targets)))

  to_number <- function(x) as.integer(factor(x, levels = nodes$name)) - 1
  
  links$value <- 1
  links$source <- to_number(links$source)
  links$target <- to_number(links$target)

  list(links = links, nodes = nodes)
}

# toSourceTarget ---------------------------------------------------------------

toSourceTarget <- function(x)
{
  x$target <- paste0(x$source, "@", x$f)
  x$caller <- ""
  lastcaller <- character()

  for (i in seq_len(nrow(x))) {
    
    L <- x$level[i]
    x$caller[i] <- if (L > 1) lastcaller[L - 1] else NA
    lastcaller[L] <- x$target[i]
  }

  kwb.utils::hsRenameColumns(x[, c("caller", "target")], list(caller = "source"))
}

# includeExclude ---------------------------------------------------------------

includeExclude <- function(x, exclude = NULL)
{
  if (! is.null(exclude)) {

    rowSelected <- ! grepl(exclude, rownames(x))
    colSelected <- ! grepl(exclude, colnames(x))

    x <- x[rowSelected, colSelected]
  }

  x
}

# relationMatrixToSourceTarget -------------------------------------------------

relationMatrixToSourceTarget <- function(x)
{
  kwb.utils::rbindAll(lapply(rownames(x), function(rowname) {
    
    if (length(names.x <- names(which(x[rowname, ] == 1)))) {
      
      data.frame(source = rowname, target = names.x)
    }
  }))
}
