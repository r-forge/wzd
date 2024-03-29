\name{inkblot}
\alias{inkblot}
\title{Plots a xts multivariate time series.}
\usage{inkblot(series, col, min.height=40, grid=TRUE, lty.grid="dashed",
    col.grid="lightgrey", col.value=24, col.category=17,
    major.ticks="auto", major.format="", num.format="", ...)
}
\description{Plots a xts multivariate time series.}
\details{It is assumed that the series values are non-negative and that the colnames of the series are set.
Creates a so-called inkblot chart.}
\value{Used for its side effect (plotting)}
\references{\url{http://stackoverflow.com/questions/2161052/how-to-create-an-inkblot-chart-with-r}}
\author{Karsten Weinert \email{k.weinert@gmx.net}}
\arguments{\item{series}{a xts object}
\item{col}{vector of colors to use (default NULL)}
\item{min.height}{minimal height of one category in the same unit as y (default 40)}
\item{grid}{logical (default TRUE); whether to draw vertical grid lines}
\item{lty.grid}{line style for grid lines (default "dashed")}
\item{col.grid}{color of grid lines (default "lightgrey")}
\item{col.value}{color for the displayed values (default 24)}
\item{col.category}{color for the category (default 17)}
\item{major.ticks}{where to put the x axis ticks and labels (default "auto")}
\item{major.format}{how to format the x axis labels (default prints the year)}
\item{num.format}{sprintf format string for the numbers (default prints without decimals places)}
\item{...}{additional parameters to plot}
}

