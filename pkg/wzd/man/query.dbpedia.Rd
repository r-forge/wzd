\name{query.dbpedia}
\alias{query.dbpedia}
\title{Extracts datasets from the DBPEDIA project.}
\usage{query.dbpedia(obj, dataset, infobox, output="data.frame", verbose=TRUE,
    maxLines=65000)
}
\description{Extracts datasets from the DBPEDIA project.}
\details{You can choose between predefined transformed datasets and your own extraction.
Furthermore, you can choose the output to be the dataset itself (as a data.frame) or
a documentation of the data in a roxygen compatible documentation string.

DBpedia is derived from Wikipedia and is distributed under the same licensing terms as Wikipedia itself. 
As Wikipedia has moved to dual-licensing, DBpedia also dual-license DBpedia starting with release 3.4.
DBpedia 3.4 data is licensed under the terms of the Creative Commons Attribution-ShareAlike 3.0 license 
and the GNU Free Documentation License. All DBpedia releases up to and including release 3.3 are licensed 
under the terms of the GNU Free Documentation License only.}
\value{depends on the output parameter}
\references{\url{http://wiki.dbpedia.org/Datasets}}
\author{Karsten Weinert \email{k.weinert@gmx.net}}
\arguments{\item{obj}{a dbpedia S3 object}
\item{dataset}{identifies the dataset. Print the dbpedia object to get a list of available entities}
\item{entity}{identifies the dataset. Print the dbpedia object to get a list of available entities}
\item{output}{define the type of output. Supported output formats "data.frame" (default) or "roxygen".}
\item{verbose}{prints a dot after every 100 000 lines so you know your machine is awake}
\item{maxLines}{parameter to readLines, maximum number of lines to read at a time (default 65000)}
}

