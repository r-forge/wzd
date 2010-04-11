\name{query.ckan}
\alias{query.ckan}
\title{Queries the CKAN data registry using its API.}
\usage{query.ckan(obj, entity, output="json", ...)
}
\description{Queries the CKAN data registry using its API.}
\details{Currently, only querying package entities is implemented.

The output is considered as UTF-8.}
\value{depending on the parameter output, a nested list, a string, or a data.frame}
\references{\url{http://knowledgeforge.net/ckan/doc/ckan/api.html}}
\author{Karsten Weinert \email{k.weinert@gmx.net}}
\arguments{\item{obj}{reference of the ckan object}
\item{entity}{specifies the entity. E.g. 'browser_stats' looks for the data package 'browser_stats' from W3Schools.}
\item{output}{determines the form of the output. 
'json' (default) returns package information as nested list
'roxygen' returns a roxygen compatible documentation string of the dataset
'url' returns a url where the data resides
'data.frame' returns the actual data}
\item{...}{unused}
}

