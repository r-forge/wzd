#' Generic S3 method to query different data sources.
#'
#' This method is implemented for various datasources of this package, see below.
#'
#' @param obj       data source object
#' @param ...       no default use
#' 
#' @return depends on the data source
#' @seealso \code{query.mediawiki} for querying mediawiki
#' @seealso \code{query.gapminder} for querying gapminder datasets
#' @seealso \code{query.ckan} for querying ckan data entities
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
query <- function(obj, ...) UseMethod("query")

