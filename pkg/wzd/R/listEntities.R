#' Generic S3 method to get information on data sources.
#'
#' This method is implemented for various datasources of this package, see below.
#'
#' @param obj       data source object
#' 
#' @return a list of strings for supported entities of the data source
#' @seealso \code{listEntities.gapminder} for gapminder datasets
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
listEntities <- function(obj, prop) UseMethod("listEntities")

