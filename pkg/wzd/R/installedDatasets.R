#' Creates a data source (S3 object) for the datasets that come with R.
#'
#' @return an object of class "installedDatasets"
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
installedDatasets <- function() {
  d <- data(package = .packages(all.available = TRUE))$results
  d <- transform(d, Labels=paste(Package, "::", Item, sep=""))
  result <- list(datasets=d)
  class(result) <- "installedDatasets"
  result
}

#' Print information on a installedDatasets data source
#'
#' Displays the number of found datasets.
#' 
#' @param x         reference of the installedDatasets object
#' @param ...       unused (from generic)
#' 
#' @return used for its side effect
#' @seealso \code{installedDatasets} for a description of the datasource
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
print.installedDatasets <- function(x, ...) {
  df <- x$datasets
  print(paste("installedDatasets data source object. ", dim(df)[1]," datasets found.", sep=""))
}
  
#' Load one installed dataset.
#'
#' @param obj         a installedDatasets S3 object
#' @param entity      identifies the dataset. A dataset name consists of the package name where it
#'                    is defined and the name of the data set, separated by two colons.
#'                    Use listEntities(installedDatasets) to get a list of available entities.
#'
#' @return A data set. The class depends on the definition of the dataset
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
query.installedDatasets <- function(obj, entity, envir=.GlobalEnv){
  print(sprintf("entity %s", entity))
  ds <- strsplit(entity, "::")[[1]]
  try(do.call("data", list(ds[[2]], package=ds[[1]], envir=envir)), silent=TRUE)
  result <- tryCatch(get(ds[[2]], envir=envir), error=function(e) NULL)
  print(sprintf("isnull(%s) = %s", entity, is.null(result)))
  return(result)
}

 
#' Data source information for installedDatasets 
#'
#' This method provides information which entities the installedDatasets API supports.
#'
#'
#' @param obj         a installedDatasets S3 object
#'
#' @return a vector of strings describing the available entities.
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}  
listEntities.installedDatasets <- function(obj) as.character(sort(obj$datasets[, "Labels"]))

#' Creates a function that creates a dataset chooser widget.
#'
#' The created function takes two parameters, tkParent (the parent for the new entry widget) and
#' env (an environment that provides access to the entry choice).
#'
#' @param varname          a name for the parameter that will be edited in the entry
#' @param filter           a function that takes a data.frame object and returns TRUE or FALSE. Only those datasets for which TRUE was
#'                         returned will be displayed. Optional, default is NULL (all datasets).
#' @param init             the initial choice on start. Optional, default is the empty string
#' @param label            a short description (max. 15 characters) of the controlled parameter. Optional, default is varname.
#' @param tip              a possibly longer description, that will (someday, currently broken) 
#'                         be displayed as a tip when the mouse is moved over the variable name.
#'
#' @return function(tkparent, env)
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
installedDataSetCombo <- function(varname, filter=NULL, init="", label=NULL, tip="") {
  # check arguments
  if(!is.character(varname)) stop("invalid varname argument, string expected.")
  if(!(is.null(filter) || is.function(filter))) stop("invalid filter argument, NULL or function expected.")
  if(!is.character(init)) stop("invalid init argument, string expected.")
  if(!is.character(tip)) stop("invalid tip argument, string expected.")
  if(is.null(label)) label <- varname

  # determine data sets
  dSets <- installedDatasets()
  choices <- NULL
  getChoices <- function() if(is.null(choices)) {
      ch <- listEntities(dSets)
      if(!is.null(filter)) {
        tempEnv <- new.env()
        filterFunc <- function(choice) {
            oneSet <- query(dSets, choice, envir=tempEnv)
            relevant <- filter(oneSet)
        }
        ch <- ch[sapply(ch, filterFunc)]  # todo - show progressbar
        rm(list=ls(envir=tempEnv), envir=tempEnv)
      }
      choices <<- ch
      choices
    } else choices
 
  # create combobox builder
  buildWidget <- function(tkParent, env) {
    resultFrame = tk2frame(tkParent)
    
    # dataset combobox
    v <- tclVar(init) 
    w <- tk2label(resultFrame, text=varname, tip=tip,  width=15, justify="right")
    tkpack(w, side="left", padx=5)
    datasetCombo <- tk2combobox(resultFrame, textvariable=v)
    tkconfigure(datasetCombo, postcommand=function() tkconfigure(datasetCombo, values=getChoices())) # not optimal
    tkpack(datasetCombo, side="left", padx=5)
    varFunc <- function() {
      e <- tclvalue(v)
      if(e!="") 
        return(query(dSets,e, envir=env))
      else
        return(NULL)
    }
    assign(varname, varFunc, envir=env)
    return(resultFrame)
  }
  return(buildWidget)
}

#' Creates a function that creates a dataset field chooser widget.
#'
#' The created function takes two parameters, tkParent (the parent for the new entry widget) and
#' env (an environment that provides access to the entry choice).
#'
#' @param varname          a name for the parameter that will be edited in the entry
#' @param init             the initial choice on start. Optional, default is the empty string
#' @param label            a short description (max. 15 characters) of the controlled parameter. Optional, default is varname.
#' @param tip              a possibly longer description, that will (someday, currently broken) 
#'                         be displayed as a tip when the mouse is moved over the variable name.
#'
#' @return function(tkparent, env)
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
fieldFromDataset <- function(varname, datasetName, factorsOnly=FALSE, label=NULL, tip="") {
  # check arguments, most checking is done in combobox
  if(!is.character(datasetName)) stop("invalid datasetName argument, string expected.")
  if(!is.logical(factorsOnly)) stop("invalid factorsOnly argument, boolean expected.")
   
  getChoices <- function(env) {
    dframe <- eval(call(datasetName), envir=env)
    choices <- colnames(dframe)
    if(factorsOnly) choices <- choices[sapply(choices, function(c) is.factor(dframe[,c]))]
    return(choices)
  }
  return(combobox(varname, choices=getChoices, label=label, tip=tip))
}
