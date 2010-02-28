\name{loadGapMinderDataset}
\alias{loadGapMinderDataset}
\title{Internal function for downloading and caching a Gapminder...}
\usage{loadGapMinderDataset(name, url, na.strings=c("..", "-"), sep.thousands="")
}
\description{Internal function for downloading and caching a Gapminder.org dataset}
\details{Downloads a dataset curated, but not provided by the gapminder project 
from google spreadsheets. 

Usually it is more convenient to use data(gmXXX) to load the dataset.
See data(package="wzd") for a list of datasets.

Please note that neither Gapminder nor the package developer/maintainer are the data provider, except for a few cases.
Therefore you will have to go to the source to find out the terms of use for the specific indicator.}
\value{a xts time series object}
\references{\url{http://www.gapminder.org}}
\author{Karsten Weinert \email{k.weinert@gmx.net}}
\arguments{\item{name}{name of the dataset}
\item{url}{the internet address of the google spreadsheet data}
\item{na.strings}{what to interpret as missing values. Defaults to c("-", "..").}
\item{sep.thousands}{optional string for thousand separator.}
}

