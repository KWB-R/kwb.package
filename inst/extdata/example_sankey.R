#
# For how to use foodweb, see
#
# http://stackoverflow.com/questions/8761857/identifying-dependencies-of-r-functions-and-scripts
#

# Plot Sankey Network ----------------------------------------------------------
if (FALSE)
{
  library("kwb.utils")
  library("kwbGompitz")
  library("kwb.rsproto")
  
  main <- function() {
    
    desktop()
  }
  
  mvbutils::foodweb(
    where = c(".GlobalEnv", "package:kwb.utils"),
    prune = "main",
    ancestors = FALSE
  )
  
  mvbutils::foodweb(
    where = kwb.package::packageString(c("kwb.rsproto", "kwb.utils")),
    prune = "catIf",
    ancestors = TRUE
  )
  
  mvbutils::foodweb(
    where = kwb.package::packageString(c("kwb.rsproto", "kwb.utils")),
    prune = "selectColumns",
    ancestors = TRUE
  )
  
  kwb.package::plotSankeyNetwork(
    "init_model",
    where = "package:kwb.rsproto"
  )
  
  kwb.package::plotSankeyNetwork(
    "main",
    where = "package:kwb.utils"
  )
  
  library("ggplot2")
  
  kwb.package::plotSankeyNetwork(
    functionName = "plot_prediction_conditions",
    where = c("package:kwb.rsprotox", "package:ggplot2"),
    descendents = TRUE,
    ancestors = TRUE
  )
}

# Example functions ------------------------------------------------------------
main <- function()
{
  f1()
  f2()
  f3()
}

f1 <- function() {
  cat("This is f1 calling desktop\n")
  kwb.utils::desktop()
}

f2 <- function() {
  cat("This is f2\n")
}

f3 <- function() {
  cat("This is f3\n")
}
