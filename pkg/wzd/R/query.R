#' Generic S3 method to query different data sources.
#'
#' CKAN describes itself as a registry of open data and content packages. CKAN provides ways to find, download and share open content and data.
#'
#' @param x data source object
#' 
#' @return depends on the data source
#' @seealso \code{query.mediawiki} for querying mediawiki
#' @seealso \code{query.gapminder} for querying gapminder datasets
#' @seealso \code{query.ckan} for querying ckan data entities
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
query <- function(x, ...) UseMethod("query")

