#' Improved list of objects, by Petr Pikal, David Hinds and Dirk Eddelbüttel.
#'
#' @param env              the environment to inspect, default is .GlobalEnv
#' @param sortBy           the result will be decreasingly sorted by this column. Possible values "Type", "Size" (default), "Rows", "Columns"
#'
#' @return data.frame with object information
#' @references 
#' \url{http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session}
#' @export
#' @author Petr Pikal, David Hinds and Dirk Eddelbüttel
mem.info <- function(env=.GlobalEnv, sortBy="Size") {
    napply <- function(names, fn) sapply(names, function(x) fn(get(x, envir=env)))
    names <- ls(envir=env)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.size <- napply(names, object.size)
    obj.dim <- t(napply(names, function(x) as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.dim)
    names(out) <- c("Type", "Size", "Rows", "Columns")
    out <- out[order(out[[sortBy]], decreasing=TRUE), ]
    return(out)
}

#' Widget for using mem.info in a gui
#'
#' @return a function(tkParent, env) for use with tabView or widgetView
#'
#' @export
#' @author Karsten Weinert
memoryWidget <- function() {
    myF <- function(envir, sortBy, runGc) {
      if(runGc) gc()
      print(mem.info(envir, sortBy))
    } 
    envVarFunc <- function(varname, env) {
      currVal <- get(sprintf("%s.getstate", varname), envir=env)()
      if(currVal=="global")
        return(.GlobalEnv)
      else
        return(env)
    }
    textManipulate(
      myF,
      c("envir", "sortBy", "runGc"),
      c(
        combobox("envir", choices=c("global", "this notebook"), init="this notebook", label="environment", tip="Select environment", varFunc=envVarFunc),
        combobox("sortBy", choices=c("Columns", "Rows", "Size", "Type"), init="Size", label="sort by", tip="sort decreasingly by?"),
        checkbox("runGc", init=FALSE, label="run gc?", tip="Run garbage collector? Usually not necessary, it runs periodically")
      )
    )
}

# todo Rprof / summaryRprof