#' Generic S3 method to get information on data sources.
#'
#' This method is implemented for various datasources of this package, see below.
#'
#' @param obj       data source object
#' @param entity    the name of the entity to be removed
#' 
#' @return Used for its side effects (modifies a data source)
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
delete <- function(obj, entity) UseMethod("delete")

