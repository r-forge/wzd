#' Generic S3 method to get information on data sources.
#'
#' This method is implemented for various datasources of this package, see below.
#'
#' @param obj       data source object
#' @param entity    the name of the entity which is submitted
#' @param data      the data of the entity
#' 
#' @return Used for its side effects (modifies a data source)
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
submit <- function(obj, entity, data) UseMethod("submit")

