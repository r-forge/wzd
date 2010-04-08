#' Internal function for downloading and caching a Gapminder.org dataset
#'
#' Downloads a dataset curated, but not provided by the gapminder project 
#' from google spreadsheets. 
#'
#' Usually it is more convenient to use data(gmXXX) to load the dataset.
#' See data(package="wzd") for a list of datasets.
#'
#' Please note that neither Gapminder nor the package developer/maintainer are the data provider, except for a few cases.
#' Therefore you will have to go to the source to find out the terms of use for the specific indicator.
#'
#' @param name name of the dataset
#' @param url the internet address of the google spreadsheet data
#' @param na.strings what to interpret as missing values. Defaults to c("-", "..").
#' @param sep.thousands optional string for thousand separator.
#' @return a xts time series object
#' @references
#' \url{http://www.gapminder.org}
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
loadGapMinderDataset <- function(name, url, na.strings=c("..", "-"), sep.thousands=""){
  if (!require(xts)) stop("Could not load required library xts")
  dataCsv <- paste(name, ".csv", sep="")
  if (!file.exists(dataCsv)) 
    if (download.file(url, dataCsv, method="internal")!=0) stop("Could not download dataset.")
    
  dataRaw <- read.csv(dataCsv, encoding="UTF-8", na.strings=na.strings, stringsAsFactor=FALSE)
  dataTransformed <- t(as.matrix(dataRaw[,2:ncol(dataRaw)]))
  if (sep.thousands!="") {
    dimSaved <- dim(dataTransformed)
    dataTransformed <- as.numeric(gsub(sep.thousands, '', dataTransformed))
    dim(dataTransformed) <- dimSaved
  }
  colnames(dataTransformed) <- dataRaw[,1]
  xts(dataTransformed, order.by=as.Date(paste(colnames(dataRaw)[2:ncol(dataRaw)],"-06-15", sep=""), format="X%Y-%m-%d"))
}
