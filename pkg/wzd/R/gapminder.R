#' Creates a Gapminder data source (S3 object).
#'
#' Gapminder describes itself as a "fact tank" that promotes a fact based world view. 
#' On their website they provide a service that allows to create animated charts for various indicators, differentiated by country.
#' They also provide the underlying datasets for download. This S3 class serves as a wrapper for easy access to a subset of these data.
#'
#' Please note that neither Gapminder nor the package developer/maintainer are the data provider, except for a few cases.
#' Therefore you will have to go to the source to find out the terms of use for the specific indicator.
#'
#' @return an object of class "gapminder"
#' @references 
#' \url{http://www.gapminder.org}
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
gapminder <- function() {
  datasets <- data.frame(
    row.names=c("gmPopulation", "gmTotalFertilityRate", "gmPerCapitaCO2Emissions", "gmIncomePerCapita", "gmInfantMortalityRate",
      "gmLifeExpectancyAtBirth", "gmAdolescentFertilityRate", "gmBirthsAttendedBySkilledHealthStaff", "gmContraceptiveUse",
      "gmCrudeBirthRate", "gmMaternalMortalityRate", "gmUnder5MortalityRate", "gmCrudeDeathRate", "gmPopulationGrowth",
      "gmSugarConsumption", "gmGDP", "gmConsumerPricesIndex", "gmGDPImplicitDeflator", "gmCoalConsumption", "gmHydroelectricityConsumption",
      "gmNaturalGasConsumption", "gmNuclearConsumption", "gmOilConsumption", "gmCoalProduction", "gmElectricityGeneration",
      "gmNaturalGasProduction", "gmOilProduction", "gmPrimaryEnergyConsumption", "gmCO2Emissions", "gmSulfurEmissions",
      "gmTotalForestArea", "gmPrimaryForestArea", "gmPlantedForestArea", "gmWoodRemoval", "gmBiomassStockInForest",
      "gmTotalWaterWithdrawal", "gmSurfaceArea", "gmBadTeethPerChild", "gmPeopleLivingWithHIV", "gmMalariaReportedCases",
      "gmMalariaReportedDeaths", "gmWorkingHoursPerWeek", "gmUrbanPopulation", "gmWomensAgeAtFirstMarriage", "gmNumberOfBillionaires",
      "gmGiniIndex", "gmBroadbandSubscribers", "gmCellPhones", "gmPersonalComputers", "gmPatentApplications", "gmPatentsGranted",
      "gmPatentsInForce", "gmArmsExports", "gmArmsImports", "gmHumanDevelopmentIndex"),
    key=c("phAwcNAVuyj0XOoBL_n5tAQ", "phAwcNAVuyj0TAlJeCEzcGQ", "phAwcNAVuyj1gkNuUEXOGag", "phAwcNAVuyj1jiMAkmq1iMg", 
      "phAwcNAVuyj0NpF2PTov2Cw", "phAwcNAVuyj2tPLxKvvnNPA", "pyj6tScZqmEdIphYUHxcdLg", "pyj6tScZqmEfKY9bk02DBYA", 
      "pyj6tScZqmEewsQOoKrtYJQ", "tUSeGJOQhafugwUvHvY-wLA", "pyj6tScZqmEcVezxiMlWaRw", "phAwcNAVuyj05ZR69usyQIg", 
      "tHyj-2jRvK3CCNJOc5Vm-HQ", "pyj6tScZqmEcl2xDWbuJ8fg", "phAwcNAVuyj2sdmdhX9zuKg", "pyj6tScZqmEfI4sLVvEQtHw", 
      "pyj6tScZqmEc3xNIyXiZ6EA", "pyj6tScZqmEcaHt8Y6cxXQg", "pyj6tScZqmEc1TmMiFdmOVg", "pyj6tScZqmEdNbX4qj9QLTA", 
      "pyj6tScZqmEcx9pD804Q0Aw", "pyj6tScZqmEfiy57wnt-tEA", "pyj6tScZqmEcm0fIa0IVtKw", "pyj6tScZqmEdDid2ts7KvHg", 
      "pyj6tScZqmEehRG-9mMHYdg", "pyj6tScZqmEfv2K6dZmskWg", "pyj6tScZqmEdNIa3ckVXaCQ", "pyj6tScZqmEeTCOezV8a3HA", 
      "phAwcNAVuyj1NHPC9MyZ9SQ", "t9SYWh7siLJDzyZYN1R4HfQ", "pp59adS3CHWeB1N1HlpFQVQ", "pp59adS3CHWeECA6Gf__BNQ", 
      "pp59adS3CHWc4aJd9fV8zZg", "pp59adS3CHWe8O-N9RgxzDw", "pp59adS3CHWcsSl830EklJA", "rIG3ZWxv381t2bIL2BNaIVw", 
      "pyj6tScZqmEeiMy8j86qDTg", "phAwcNAVuyj3Os9LVO_pRDA", "pyj6tScZqmEe1GaiYJX2qGA", "pp59adS3CHWczfPHQMiqxCg", 
      "pp59adS3CHWfZGL9qouvTbQ", "rIMebcn9Eo2jSIm09HBLihg", "pyj6tScZqmEfH89V6UQhpZA", "t4eF8H_jq_xyKCUHAX6VT1g", 
      "tNWhbu-1UIPPxtmRHtnINOQ", "pyj6tScZqmEcjeKHnZq6RIg", "pyj6tScZqmEcuy6dYkzGhfw", "pyj6tScZqmEcKuNdFCUo6TQ", 
      "pyj6tScZqmEfUXdC83YSzfw", "pyj6tScZqmEd5FA9xlfO9eA", "pyj6tScZqmEdMioz5VJKXHw", "pyj6tScZqmEe371ZVZl73eA", 
      "pyj6tScZqmEeTIhjRrVQtQA", "pyj6tScZqmEfnPl7VRfT9WA", "tyadrylIpQ1K_iHP407374Q"),
    dataGid=0,
    docGid=c(2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), 
    stringsAsFactors=FALSE)
  result <- list(api="http://spreadsheets.google.com/pub?key=", datasets=datasets)
  class(result) <- "gapminder"
  
  result
}

#' Print information on a gapminder S3 object
#'
#' Displays the names of available entities. For documentation of a specific dataset see the wzddata package.
#' 
#' @param x         reference of the gapminder object
#' @param ...         unused (from generic)
#' 
#' @return used for its side effect
#' @seealso \code{gapminder} for a description of the datasource
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
print.gapminder <- function(x, ...) {
  df <- x$datasets
  print(paste("Gapminder data source object. ", dim(df)[1]," datasets available:", sep=""))
  print(strwrap(Reduce(function(x,y) paste(x,y, sep=", "), sort(rownames(df)))))
}
  
#' Downloads a dataset curated by Gapminder.
#'
#' You can choose if you want the data as a data.frame or as a xts time series, of if
#' you want the documentation of the data in a roxygen compatible documentation string.
#'
#' Please note that neither Gapminder nor the package developer/maintainer are the data provider, except for a few cases.
#' Therefore you will have to go to the source to find out the terms of use for the specific indicator.
#'
#' @param obj         a gapminder S3 object
#' @param entity    identifies the dataset. Print the gapminder object to get a list of available entities
#' @param output      define the type of output. Supported output formats "data.frame" (default), "xts" or "roxygen".
#' @param ...         unused
#'
#' @return depends on the output parameter
#' @references
#' \url{http://www.gapminder.org}
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
query.gapminder <- function(obj, entity, output="data.frame", ...){
  r <- try(obj$datasets[entity,], silent=TRUE)
  if (class(r)=="try-error") stop("Entity not found.")
  
  theUrl <- paste(obj$api,r$key, "&output=csv","&gid=", sep="")
  theUrl <- paste(theUrl, if (output=="roxygen") r$docGid else r$dataGid, sep="")

  if (output=="xts") {
    if (!require(xts)) stop("Could not load required library xts")
    dataRaw <- read.csv(theUrl, encoding="UTF-8", na.strings=c("..", "-"), stringsAsFactor=FALSE)
    dataTransformed <- t(as.matrix(dataRaw[,2:ncol(dataRaw)]))
    if (!is.numeric(dataTransformed[1,])) {
      dimSaved <- dim(dataTransformed)
      dataTransformed <- as.numeric(gsub("[,']", '', dataTransformed))
      dim(dataTransformed) <- dimSaved
    }
    colnames(dataTransformed) <- dataRaw[,1]
    return(xts(dataTransformed, order.by=as.Date(paste(colnames(dataRaw)[2:ncol(dataRaw)],"-06-15", sep=""), format="X%Y-%m-%d")))
  }
  
  if (output=="data.frame") {
    dataRaw <- read.csv(theUrl, encoding="UTF-8", na.strings=c("..", "-"), stringsAsFactor=TRUE)
    transformYear <- function(s) as.numeric(substring(s,2))
    #return(dataRaw)
    dataTransformed <- do.call("rbind", lapply(colnames(dataRaw)[-1], function(x) {data.frame(dataRaw[,1], transformYear(x), dataRaw[,x])}))
    #return(dataTransformed)
    names(dataTransformed) <- c("Country", "Year", "Value")
    if (!is.numeric(dataTransformed[,3])) {
      transformVal <- function(s) as.numeric(gsub("[',]", '', s))
      dataTransformed <- transform(dataTransformed, Value=transformVal(Value))
    } 
    names(dataTransformed) <- c("Country", "Year", entity)
    return(dataTransformed)
  }
  
  if (output=="roxygen") {
    docData <- read.csv(theUrl, header=FALSE, encoding="UTF-8", stringsAsFactor=FALSE)[,c(2,3)]
    transformRow <- function(docRow) 
      if(docRow[2]=="") 
        c("#' ", paste("#' ",docRow[1], sep=""))
      else 
        strwrap(paste(docRow[1], ": ", docRow[2], sep=""), prefix="#' ")
    docData <- unlist(apply(docData, 1, transformRow))
    docData <- docData[2:length(docData)]
    docData <- c(
      docData,
      "#' ",
      "#' Please note that neither Gapminder nor the package developer/maintainer are the data provider, except for a few cases.",
      "#' Therefore you will have to go to the source to find out the terms of use for the specific indicator.",
      "#' ",
      paste("#' @name ", entity, "-data", sep=""),
      paste("#' @aliases ", entity, sep=""),
      "#' @references",
      "#' \\url{http://www.gapminder.org}",
      paste("#' \\url{",theUrl,"}", sep=""),
      "#' @docType data",
      "#' @author Karsten Weinert \\email{k.weinert@@gmx.net}")
    docData <- c(docData,
      paste(entity, " <- local({", sep=""),
      '  if (!require(wzd)) stop("Could not load required library wzd (install from r-forge)")',
      paste('  query(gapminder(), "', entity, '", output="data.frame")', sep=""),
      "})",
      "")
    return(paste(docData, sep="\n"))
  }
}
