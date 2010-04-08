\name{query.gapminder}
\alias{query.gapminder}
\title{Downloads a dataset curated by Gapminder.}
\usage{query.gapminder(obj, entity, output="data.frame")
}
\description{Downloads a dataset curated by Gapminder.}
\details{You can choose if you want the data as a data.frame or as a xts time series, of if
you want the documentation of the data in a roxygen compatible documentation string.

Please note that neither Gapminder nor the package developer/maintainer are the data provider, except for a few cases.
Therefore you will have to go to the source to find out the terms of use for the specific indicator.}
\value{depends on the output parameter}
\references{\url{http://www.gapminder.org}}
\author{Karsten Weinert \email{k.weinert@gmx.net}}
\arguments{\item{obj}{a gapminder S3 object}
\item{entity}{identifies the dataset. Print the gapminder object to get a list of available entities}
\item{output}{define the type of output. Supported output formats "data.frame" (default), "xts" or "roxygen".}
}

