#' Creates a CKAN data source (S3 object).
#'
#' CKAN describes itself as a registry of open data and content packages. CKAN provides ways to find, download and share open content and data.
#'
#' @param api string pointing to the api url as parameter. Default is http://ckan.net
#' 
#' @return an object of class "ckan"
#' @references 
#' \url{http://knowledgeforge.net/ckan/doc/ckan/api.html}
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
#' @TODO define datasets - nice tags: guardian, airport, format-csv, oft genug muss nur der Eintrag korrigiert werden
ckan <- function(api="http://ckan.net") {
  result <- list(api=api)
  class(result) <- "ckan"
  
  result
}

#' String representation of a ckan object
#'
#' Displays the API url.
#' 
#' @param obj         reference of the ckan object
#' 
#' @return string
#' @seealso \code{ckan} for a description of the datasource
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
print.ckan <- function(obj, ...) print(paste("CKAN data source object (", obj$api,")", sep=""))
  
#' Queries the CKAN data registry using its API.
#'
#' Currently, only querying package entities is implemented.
#' 
#' The output is considered as UTF-8.
#'
#' @param entity      specifies the entity. E.g. "browser_stats" looks for the data package "browser_stats" from W3Schools.
#' @param output      determines the form of the output. 
#'                    "json" (default) returns package information as nested list
#'                    "roxygen" returns a roxygen compatible documentation string of the dataset
#'                    "url" returns a url where the data resides
#'                    "data.frame" returns the actual data
#' 
#' @return depending on the parameter output, a nested list, a string, or a data.frame
#' @references 
#' \url{http://knowledgeforge.net/ckan/doc/ckan/api.html}
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
query.ckan <- function(obj, entity, output="json" ) {
  if (!require(RJSONIO)) error("Could not load required library RJSONIO (install from Omegahat)")
  url <- paste(obj$api, "/api/rest/package/", entity, sep="")
  res <- fromJSON(url)

  setEncoding <- function(x) {
    if (class(x)=="list") x <- sapply(x, setEncoding)
    if (class(x)=="character") Encoding(x) <- "UTF-8"
    x
  }  
  
  if (output=="json") return(setEncoding(res))
  dataURL <- res$download_url
  if (output=="url") return(dataURL)
  if (output=="data.frame") return(read.csv(dataURL))
  if (output=="roxygen") {
    dataName <- make.names(paste("ckan.", res$name, sep=""))
    lines <- c(
      res$title, 
      "", 
      "This is a dataset accessed at the Comprehensive Knowledge Archive Network (CKAN).",
      "Below extracts of the meta information. See the webpage referenced for more details.",
      "",
      if (!is.null(res$license)) c("License",res$license, ""),
      if (!is.null(res$notes)) c("Notes", res$notes, ""),
      if (!is.null(res$version)) c("Version", res$version, ""),
      paste("@name ", dataName, "-data", sep=""),
      paste("@aliases ", dataName, sep=""),
      "@references",
      paste("\\url{", res$download_url, sep=""),
      paste("\\url{", res$ckan_url, sep=""),
      "@docType data",
      paste("@author ", gsub("@", "@@", res$author), sep=""))
    lines <- sapply(lines, function(s) paste("#' ", s, sep=""))
    lines <- c(lines,
      paste(dataName, " <- local({", sep=""),
      '  if (!require(wzd)) stop("Could not load required library wzd (install from r-forge)")',
      paste('  query(ckan(), "', entity, '", output="data.frame")', sep=""),
      "})",
      "")
    return(paste(lines, sep="\n"))
  }
}
