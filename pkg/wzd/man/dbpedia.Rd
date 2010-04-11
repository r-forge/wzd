\name{dbpedia}
\alias{dbpedia}
\title{Creates a dbpedia S3 object.}
\usage{dbpedia(api="")
}
\description{Creates a dbpedia S3 object.}
\details{DBpedia describes itself as "a community effort to extract structured information from Wikipedia and to make this information 
available on the Web". DBpedia provides a webservice to run SPARQL queries against wikipedia and it provides several datasets
extracted from the wikipedia for download.

The S3 class dbpedia wraps access to the downloadable datasets and includes some clean-up mechanisms. The datasets are quite
large (>40 MB compressed), that is why working with a local copy of the data is supported.

DBpedia is derived from Wikipedia and is distributed under the same licensing terms as Wikipedia itself. 
As Wikipedia has moved to dual-licensing, DBpedia also dual-license DBpedia starting with release 3.4.
DBpedia 3.4 data is licensed under the terms of the Creative Commons Attribution-ShareAlike 3.0 license 
and the GNU Free Documentation License. All DBpedia releases up to and including release 3.3 are licensed 
under the terms of the GNU Free Documentation License only.}
\value{an object of class "dbpedia"}
\references{\url{http://wiki.dbpedia.org/Downloads34}}
\author{Karsten Weinert \email{k.weinert@gmx.net}}
\arguments{\item{api}{string pointing to download directory. Default is first to look at options('wzd.dbpedia.api') and,
if unset or empty, point to http://downloads.dbpedia.org/3.4/}
}

