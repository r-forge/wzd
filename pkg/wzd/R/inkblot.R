#' Plots a xts multivariate time series.
#'
#' It is assumed that the series values are non-negative and that the colnames of the series are set.
#' Creates a so-called inkblot chart.
#'
#' @param series a xts object
#' @param col vector of colors to use (default NULL)
#' @param min.height minimal height of one category in the same unit as y (default 40)
#' @param grid logical (default TRUE); whether to draw vertical grid lines
#' @param lty.grid line style for grid lines (default "dashed")
#' @param col.grid color of grid lines (default "lightgrey")
#' @param col.value color for the displayed values (default 24)
#' @param col.category color for the category (default 17)
#' @param major.ticks where to put the x axis ticks and labels (default "auto")
#' @param major.format how to format the x axis labels (default prints the year) 
#' @param num.format sprintf format string for the numbers (default prints without decimals places)
#' @param ... additional parameters to plot
#' @return Used for its side effect (plotting)
#' @references
#' \url{http://stackoverflow.com/questions/2161052/how-to-create-an-inkblot-chart-with-r}
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
#' @todo use strheight
inkblot <- function(series, col=NULL, min.height=40, grid=TRUE, lty.grid="dashed", 
                    col.grid="lightgrey", col.value=24, col.category=17, 
                    major.ticks = "auto", major.format="",
                    num.format = "", ...) {  
  # assumes non-negative values  
  # assumes that series is multivariate series  
  # assumes that series names are set, i.e. colnames(series) != NULL  

  oldpar = par(no.readonly = TRUE); on.exit(par(oldpar))
  par(mar=c(3,3,0,10)+0.1, cex=0.7)  
  
  # hack for roxygen
  if (major.format=="") major.format="%Y" 
  if (num.format=="") num.format = "%5.0f"

  if(length(col)==0){  
    col <- rainbow(dim(series)[2])  
  }  

  ytotal <- 0  
  for(category in colnames(series)) {  
    y <- series[, category]
    y <- y[!is.na(y)]
    ytotal <- ytotal + max(y, min.height)  
  }  

  x <- xy.coords(.index(series), series[, 1])$x
  plot(x, 1:length(x), type="n", ylim=c(0,1)*ytotal, yaxt="n", xaxt="n", bty="n", ylab="", xlab="", ...) 
  ep <- axTicksByTime(series, major.ticks, format = major.format, format)
  axis(side=1, at = x[ep], labels = names(ep), las = 1, lwd = 1, mgp = c(3, 1, 0))
  if (grid) abline(v=x[ep], col=col.grid, lty=lty.grid)

  catNumber <- 1  
  offset <- 0  
  for(category in rev(colnames(series))) {  
    y <- 0.5 * as.vector(series[,category])  
    offset <- offset + max(max(abs(y[!is.na(y)])), 0.5*min.height)  
    polygon(c(x, rev(x)), c(offset+y, offset-rev(y)), col=col[catNumber], border=NA)  
    mtext(text=sprintf(num.format, y[1]), side=2, at=offset, las=2, cex=0.7, col=col.value)  
    mtext(text=sprintf(num.format, y[length(y)]), side=4, line=2, at=offset, las=2, adj=1, cex=0.7, col=col.value)  
    mtext(text=paste(" ",category, sep=""), side=4, line=2, at=offset, las=2, adj=0, cex=0.7, col=col.category)  
    offset <- offset + max(max(abs(y[!is.na(y)])), 0.5*min.height)  
    catNumber <- catNumber + 1   
  }  
}  

