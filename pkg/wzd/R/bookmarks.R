#' Creates a data source (S3 object) for bookmarks created with the notebook function.
#'
#' @return an object of class "bookmarks"
#'
#' @param nbTitle      the title of the notebook
#' @param fileName     a R data file (created with save()) that contains bookmark datasets
#'
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
bookmarks <- function(nbTitle, fileName) {
  nbVarname <- gsub("[^[:alnum:]]", "",nbTitle)
  bmDataFrame <- data.frame()
  local({
        load(fileName)
        bmDataFrame <<- nbVarname
     })
  result <- list(dataset=bmDataFrame, nbTitle=nbTitle, nbVarname=nbVarname, fileName=fileName)
  class(result) <- "bookmarks"
  result
}

#' Print information on a bookmarks data source
#' 
#' @param x         reference of the bookmarks object
#' @param ...       unused (from generic)
#' 
#' @return used for its side effect
#' @seealso \code{bookmarks} for a description of the datasource
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
print.bookmarks <- function(x, ...) {
  df <- x$dataset
  print(sprintf("%d bookmarks for the notebook '%s'.", dim(df)[1], x$nbTitle))
}
  
#' Load one bookmark.
#'
#' @param obj         a bookmarks S3 object
#' @param entity      identifies the dataset. A dataset name consists of the package name where it
#'                    is defined and the name of the data set, separated by two colons.
#'                    Use listEntities(bookmarks) to get a list of available entities.
#'
#' @return A data.frame or NULL in case the requested bookmark is not found.
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
query.bookmarks <- function(obj, entity){
  result <- subset(obj$dataset, bmTitle==entity)
  if(dim(result)[1]==0) result <- NULL
  return(result)
}
 
#' Available bookmarks
#'
#' This method provides a list of the titles of the stored bookmarks
#'
#' @param obj         a bookmarks S3 object
#'
#' @return a vector of strings describing the available entities.
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}  
listEntities.bookmarks <- function(obj) {
  result <- subset(obj$dataset, select=bmTitle)
  if(dim(result)[1]==0) 
    result <- NULL
  else
    result <- as.vector(result[,1])
  return(result)
}

submit.bookmarks <- function(obj, entity, data) {
  # todo - check if data is valid
  obj$dataset <- rbind(obj$dataset, data)
  local({
      load(fileName)
      assign(obj$nbVarname, nbVarname)
      save(list=ls(), obj$fileName)
   })
}