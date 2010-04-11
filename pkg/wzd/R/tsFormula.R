

tsFormula <- function(expr, parents, title="") {
  res = list(expr=expr, parents=parents, title="")
  class(res) <- "tsFormula"
  res
}

window.tsFormula <- function(z, ...) {
  Reduce(z$expr, lapply(z$parents, window, ...))
}

# library(igraph)
# g <- graph.empty()
# g <- add.vertices(g, 4, 
    # label=c('a', 'b', 'c', 'd'), 
    # shape=c('rectangle', 'rectangle', 'circle', 'circle'),
    # color=c("red", "red", "blue", "green"),
    # size=c(4,4,4,4),
    # frame.color=c("blue", "blue", "red", "black"),
    # label.dist=c(0.2, 0.5, 1,2)
    # )
# g <- add.edges(g, c(1, 2, 2, 3))
# plot(g)
# plot(g, layout=layout.reingold.tilford)
# parameter für add.edges -- curved (TRUE, FALSE), arrow.size (number), lty, color, arrow.mode, label.family="serif", label.cex, width
# parameter für add.vertices - label, shape, colr, size, frame.color, label.dist, label.degree, label.family="serif", label.cex, label.font
# http://lists.gnu.org/archive/html/igraph-help/2010-02/threads.html
as.igraph.tsFormula <- function(z, parentVertex=-1, g=NULL, color.expr="white", color.parents="grey") {
  if (!require(igraph)) stop("Could not load required library igraph")
  theGraph <- if (!is.null(g)) g else graph.empty(directed=FALSE)
  lenParents <- length(z$parents)
  labelExpr <- if (nchar(z$expr)==1) z$expr else "f"
  labelParents <- names(z$parents)
  theGraph <- add.vertices(theGraph, 1+lenParents, label=c(labelExpr, labelParents), shape=c("circle", rep("rectangle", lenParents)))
  edges <- rep(parentVertex+1, lenParents*2)
  edges[1:lenParents * 2] <- seq(parentVertex+2, parentVertex+1+lenParents)
  theGraph <- add.edges(theGraph, edges)
  theGraph
}

plot.tsFormula <- function(z) {
  g <- as.igraph.tsformula(z)
  topDownLayout <- layout.reingold.tilford(g)
  bottUpLayout <- c(topDownLayout[,1], -(topDownLayout[,2]-max(topDownLayout[,2])))
  dim(bottUpLayout) <- dim(topDownLayout)
  plot(g, layout=bottUpLayout)
}

runMe <- function() {
  x <- ts(rnorm(30), frequency = 1, start = 1959)
  y <- ts(rnorm(30), frequency = 1, start = 1959)
  z <- tsFormula("+", parents=list(x=x, y=y), title="z")
  plot.tsFormula(z)
}