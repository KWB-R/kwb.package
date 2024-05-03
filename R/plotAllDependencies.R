# plotAllDependencies ----------------------------------------------------------

#' Plot all Package Dependencies
#' 
#' @param dependencies list of package dependencies as returned by 
#'   \code{\link{packageDependencies}}
#' @param r radius of the unit circle in which to arrange the package names.
#'   Passed to \code{\link{plotNodes}}
#' @param for.each if \code{TRUE} (default) not only an overview plot showing
#'   all dependencies but also one plot per package of which dependency
#'   information are contained in \code{dependencies} is created.
#' @param \dots arguments passed to \code{\link{plotDependencies}}
#' 
#' @export
#' 
#' @examples 
#' kwb.packages <- installedKwbPackages()
#'   
#' # Plot all (direct and indirect) dependencies of installed kwb packages
#' plotAllDependencies(packageDependencies(kwb.packages))
#'   
#' # Plot only direct dependencies
#' plotAllDependencies(packageDependencies(kwb.packages, recursive = FALSE))
#' 
plotAllDependencies <- function(dependencies, r = 1.5, for.each = TRUE, ...)
{
  packageNames <- unique(c(names(dependencies), unlist(dependencies)))
  
  nodes <- toNodes(nodeNames = packageNames)  
  
  plotDependencies(
    nodes, dependencies, 
    main = paste("Dependencies of all kwb.packages"), 
    r = r,
    ...
  )
  
  if (for.each) {
    
    for (nodeName in names(dependencies)) {
      
      plotDependencies(
        nodes, dependencies[nodeName], 
        main = paste("Dependencies of", nodeName), 
        r = r,
        nodeColours = "black",
        ...
      )
    }
  }
} 

# toNodes ----------------------------------------------------------------------

#' Nodes in a Unit Circle
#' 
#' Node names to node coordinates in a unit circle
#' 
#' @param nodeNames character vector of nodes to be arranged in a unit circle
#' 
#' @return data frame with columns \code{x} and \code{y} giving the coordinates
#'   of the nodes arranged in a circle. The row names represent the node names.
#' 
#' @export
#' 
#' @examples 
#' nodes <- toNodes(LETTERS)
#' 
#' plot(nodes)
#' text(nodes, labels = row.names(nodes), adj = c(0, 0))
#'   
toNodes <- function(nodeNames)
{
  n_nodes <- length(nodeNames)
  
  nodes <- anglesToPoints(equidistantAngles(n_nodes))
  
  rownames(nodes) <- nodeNames
  
  nodes
}

# plotDependencies -------------------------------------------------------------

#' Plot Dependencies Between Nodes on a Circle Line
#' 
#' @param nodes data frame as returned by \code{\link{toNodes}}
#' @param dependencies list of package dependencies as returned by
#'   \code{\link{packageDependencies}}
#' @param main plot title
#' @param r radius of the circle
#' @param nodeColours colours to be given to the nodes
#' @param \dots passed to \code{\link{drawDependencies}}
#' @importFrom grDevices rainbow
#' @export
#' 
plotDependencies <- function(
  nodes, 
  dependencies,
  main = "", 
  r = 1.5, 
  nodeColours = grDevices::rainbow(nrow(nodes)),
  ...
)
{
  nodeColours <- rep(nodeColours, length.out = nrow(nodes))
  
  plotNodes(nodes, r = r, col = nodeColours, main = main)
  addNodeLabels(nodes, cex = 0.8, distance.factor = 1.03)
  drawDependencies(nodes, dependencies, nodeColours, ...)
}

# plotNodes --------------------------------------------------------------------

#' Plot Nodes
#' 
#' @param nodes data frame as returned by \code{\link{toNodes}}
#' @param r Radius of a circle that fits into the plot range: xlim is set to 
#'   \code{c(-r, r)}
#' @param col colour used to plot the nodes (default: "red")
#' @param \dots arguments passed to \code{plot}
#' @export
#' @importFrom graphics plot 
plotNodes <- function(nodes, r = 1, col = "red", ...)
{
  graphics::plot(
    nodes, xlim = c(-r, r), asp = 1, axes = FALSE, xlab = "", ylab = "", 
    pch = 16, col = col, ...
  )
}

# addNodeLabels ----------------------------------------------------------------

#' Add Labels to the Nodes Drawn on a Circle Line
#' 
#' @param nodes data frame as returned by \code{\link{toNodes}}
#' @param cex character expansion factor as given to \code{text}
#' @param distance.factor expansion factor applied to the x and y coordinates of the nodes to get 
#'   the coordinates of the labels
#' @export
addNodeLabels <- function(nodes, cex = 1, distance.factor = 1)
{
  rowNames <- row.names(nodes)
  
  cos45 <- cos(gradToRad(45))
  
  for (i in seq_len(nrow(nodes))) {
    
    x <- nodes[i, "x"] * distance.factor
    y <- nodes[i, "y"] * distance.factor
    
    vertical <- abs(x) < cos45
    
    adj <- ifelse((ifelse(vertical, y, x)) > 0, 0, 1)
    srt <- ifelse(vertical, 90, 0)
    
    labels <- rowNames[i]
    
    graphics::text(
      x = x, y = y, labels = labels, adj = adj, cex = cex, srt = srt
    )
  }  
}

# drawDependencies -------------------------------------------------------------

#' Draw Lines between Nodes
#' 
#' @param nodes data frame as returned by \code{\link{toNodes}}
#' @param dependencies list of package dependencies as returned by 
#'   \code{\link{packageDependencies}}
#' @param nodeColours colours given to the lines starting at the same start node
#' @param \dots arguments passed to \code{\link{drawLink}}
#' @export
#' 
drawDependencies <- function(nodes, dependencies, nodeColours, ...)
{
  nodeNames <- rownames(nodes)
  
  startNodes <- names(dependencies)  
  
  for (startNode in startNodes) {
    
    stopNodes <- dependencies[[startNode]]        
    
    for (stopNode in stopNodes) {
      
      drawLink(
        nodes, startNode, stopNode, col = nodeColours[startNode == nodeNames],
        ...
      )
    }    
  }  
}

# drawLink ---------------------------------------------------------------------

#' Draw a Link Between a Pair of Nodes
#' 
#' @param nodes data frame as returned by \code{\link{toNodes}}
#' @param i row index of the start node
#' @param j row index of the finish node
#' @param \dots arguments passed to \code{arrows}
#' @export
#' @importFrom graphics arrows
drawLink <- function(nodes, i, j, ...) 
{
  graphics::arrows(
    x0 = nodes[i, "x"], y0 = nodes[i, "y"], 
    x1 = nodes[j, "x"], y1 = nodes[j, "y"], ...
  )
}
