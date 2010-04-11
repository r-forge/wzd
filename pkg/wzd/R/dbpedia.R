#' Creates a dbpedia S3 object.
#'
#' DBpedia describes itself as "a community effort to extract structured information from Wikipedia and to make this information 
#' available on the Web". DBpedia provides a webservice to run SPARQL queries against wikipedia and it provides several datasets
#' extracted from the wikipedia for download.
#'
#' The S3 class dbpedia wraps access to the downloadable datasets and includes some clean-up mechanisms. The datasets are quite
#' large (>40 MB compressed), that is why working with a local copy of the data is supported.
#'
#' DBpedia is derived from Wikipedia and is distributed under the same licensing terms as Wikipedia itself. 
#' As Wikipedia has moved to dual-licensing, DBpedia also dual-license DBpedia starting with release 3.4.
#' DBpedia 3.4 data is licensed under the terms of the Creative Commons Attribution-ShareAlike 3.0 license 
#' and the GNU Free Documentation License. All DBpedia releases up to and including release 3.3 are licensed 
#' under the terms of the GNU Free Documentation License only.
#'
#' @param api string pointing to download directory. Default is first to look at options('wzd.dbpedia.api') and,
#'            if unset or empty, point to http://downloads.dbpedia.org/3.4/
#' 
#' @return an object of class "dbpedia"
#' @references 
#' \url{http://wiki.dbpedia.org/Downloads34}
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
dbpedia <- function(api="") {
  if (api=="") api <- getOption("wzd.dbpedia.api", "http://downloads.dbpedia.org/3.4")
  datasets <- data.frame(
    row.names=c("infobox_de", "infobox_en"),
    file=c("de/infobox_de.csv.bz2", "en/infobox_en.csv.bz2"),
    stringsAsFactors=FALSE)
  entities <- c("wpCompanies")
  keymap <- data.frame(
    entity="wpCompanies",
    dataset="infobox_de",
    key=c("sitz", "homepage","branche", "isin", "unternehmensform", "leitung", "name", "gr_percent_E3_percent_BCndungsdatum", 
          "mitarbeiterzahl", "produkte", "umsatz", "bilanzsumme", "aufl_percent_E3_percent_B6sungsdatum", "gewinn"),
    newkey=c("location", "homepage", "industry", "isin", "type", "CEO", "name", "founded", "num_employees", "products", "revenue", 
          "assets", "defunct", "net_income"),
    stringsAsFactors=FALSE)
  result <- list(api=api, datasets=datasets, entities=entities, keymap=keymap)
  class(result) <- "dbpedia"
  
  result
}

#' String representation of a dbpedia object
#'
#' Displays the names of available entities. For documentation of a specific dataset see the wzddata package.
#' 
#' @param x           reference of the dbpedia object
#' @param ...         unused (from generic)
#' 
#' @return string
#' @seealso \code{dbpedia} for a description of the datasource
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
print.dbpedia <- function(x, ...) {
  cat("Dbpedia data source object. \n")
  df <- rownames(x$datasets)
  cat(paste("\n", length(df)," datasets defined:\n", sep=""))
  cat(df)
  cat("\n")
  df <- x$entities
  cat(paste("\n", length(df)," predefined entities available:\n", sep=""))
  cat(df)
  cat("\n")
  #print(strwrap(Reduce(function(x,y) paste(x,y, sep=", "), sort(df))))
}

#' Extracts datasets from the DBPEDIA project.
#'
#' You can choose between predefined transformed datasets and your own extraction.
#' Furthermore, you can choose the output to be the dataset itself (as a data.frame) or
#' a documentation of the data in a roxygen compatible documentation string.
#'
#' DBpedia is derived from Wikipedia and is distributed under the same licensing terms as Wikipedia itself. 
#' As Wikipedia has moved to dual-licensing, DBpedia also dual-license DBpedia starting with release 3.4.
#' DBpedia 3.4 data is licensed under the terms of the Creative Commons Attribution-ShareAlike 3.0 license 
#' and the GNU Free Documentation License. All DBpedia releases up to and including release 3.3 are licensed 
#' under the terms of the GNU Free Documentation License only.
#'
#' @param obj       a dbpedia S3 object
#' @param dataset   identifies the dataset. Print the dbpedia object to get a list of available entities
#' @param infobox    identifies the dataset. Print the dbpedia object to get a list of available entities
#' @param output    define the type of output. Supported output formats "data.frame" (default) or "roxygen".
#' @param verbose   prints a dot after every 100 000 lines so you know your machine is awake
#' @param maxLines  parameter to readLines, maximum number of lines to read at a time (default 65000)
#' @param ...       unused
#'
#' @return depends on the output parameter
#' @references
#' \url{http://wiki.dbpedia.org/Datasets}
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
query.dbpedia <- function(obj, dataset, infobox=NULL, output="data.frame", verbose=TRUE, maxLines=65000, ...){
  ds <- try(obj$datasets[dataset, "file"], silent=TRUE)
  if (class(ds)=="try-error") stop(paste("Dataset ", ds, " not found.", sep=""))
  infile <- file.path(obj$api, ds)
  
  if (!is.null(infobox) && output=="data.frame") {
    reader <- bzfile(infile, "r")
    writer <- textConnection("queryRes", open = "w", local = TRUE)
    writeLines("thing\tkey\tvalue\tetc\n", writer)

    oldthing <- ""
    hasInfobox <- FALSE
    lineNumber <- 0
    matches <- 0
    key <- ""
    thing <- ""
    
    repeat {
      lines <- readLines(reader, maxLines)
      if (length(lines)==0) break
      #if (lineNumber > 20000) break
      for (line in lines) {
        lineNumber <- lineNumber + 1
        if (lineNumber %% 100000 == 0) cat(".")
        row = unlist(strsplit(line, "\t"))
        if (length(row)>0) thing <- row[1]
        #if (lineNumber > 20000) break
        #if (thing=="3M") print (line)
        if (length(row)>1) key <- row[2]
        if (length(row)>2) value <- row[3]
        if ((length(row)>0) && (oldthing != thing)) {
          if (hasInfobox) {
            matches <- matches + 1
            writeLines(buf, writer)
          }
          buf <- c()
          hasInfobox <- FALSE
        }
        hasInfobox <- hasInfobox || ((tolower(key)=="wikipageusestemplate") && (tolower(value)==tolower(infobox)))
        #if thing=="3M": print "hasInfobox? ", hasInfobox
        oldthing <- thing
        buf <- c(buf, line)
      }
    }
    close(reader)
    close(writer)
    readRes <- textConnection(queryRes, "r")
    result <- read.csv(readRes, sep="\t", stringsAsFactors=FALSE)
    close(readRes)
    result
  }
}

# dbpedia1 <- function() {
  # data <- read.csv("C:\\Dokumente und Einstellungen\\TravelMate\\Desktop\\infobox_de.csv\\companies-R.csv", sep="\t")
  # t <- as.list(table(data$Key))
  # t <- t[which(t>1)]
  # t <- data.frame(Key=names(t), Freq=as.vector(t, mode="numeric"))
  # t[rev(order(t$Freq)),]
# }
