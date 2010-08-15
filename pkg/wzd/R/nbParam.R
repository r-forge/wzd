
notImplementedYet <- function(tkParent, env) {
  lab <- tk2label(tkParent, text="Not implemented yet.")
  return(lab)
}

#' Creates a function that creates a slider widget.
#'
#' The created function takes two parameters, tkParent (the parent for the new slider widget) and
#' env (an environment that provides access to the slide parameter).
#'
#' @param varname          a name for the parameter that will be controlled by a slider
#' @param from             the minimum allowed value
#' @param to               the maximum allowed value
#' @param init             the initial value of the parameter. Optional, default is the mean of from and to.
#' @param digits           rounds the parameter to a multiple of 10^(-digits). If digits=<0, an integer is returned. Default value NULL does not modifications.
#' @param label            a short description (max. 15 characters) of the controlled parameter. Optional, default is varname.
#' @param tip              a possibly longer description, that will (someday, currently broken) 
#'                         be displayed as a tip when the mouse is moved over the variable name.
#'
#' @return function(tkparent, env)
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
slider <- function(varname, from, to, init=NULL, digits=NULL, label=NULL, tip="") {
  # check arguments
  if(!is.character(varname)) stop("invalid varname argument, string expected.")
  if(!is.numeric(from)) stop("invalid from argument, number expected.")
  if(!is.numeric(to)) stop("invalid to argument, number expected.")
  if(is.null(init)) init <- (to+from)/2
  if(!is.numeric(init) || init<from || init>to) stop(sprintf("invalid init argument, NULL or number between %f and %f expected.", from, to))
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  if(!(is.null(digits) || is.wholenumber(digits))) stop("invalid digits argument, whole number or NULL expected.")
  if(!(is.null(label) || is.character(label))) stop("invalid label argument, string or NULL expected.")
  if(is.null(label)) label <- varname
  if(!is.character(tip)) stop("invalid tip argument, string expected.")
  
  buildWidget <- function(tkParent, env) {
    f <- tk2frame(tkParent)
    v <- tclVar(init)
    w <- tk2label(f, text=label, tip=tip,  width=15, justify="right")
    tkpack(w, side="left", padx=5)
    w <- tk2scale(f, from=from, to=to, orient="horizontal", variable=v, takefocus=0)
    if(!is.null(digits)) {
      updater <- function(...) {
        tclvalue(v) <- round(as.numeric(tclvalue(v))*10^digits) / 10^digits
      }
      tkconfigure(w, command=updater)
      updater()
    }
    tkpack(w, side="left", padx=5)
    w <- tk2entry(f, width=5, textvariable=v, state="disabled", takefocus=0)
    tkpack(w, side="left", padx=5)
    if(is.null(digits) || (digits > 0))
      assign(varname, function() as.numeric(tclvalue(v)), envir=env)
    else
      assign(varname, function() as.integer(tclvalue(v)), envir=env)
    return(f)
  }
  return(buildWidget)
}

#' Creates a function that creates a combobox widget.
#'
#' The created function takes two parameters, tkParent (the parent for the new combobox widget) and
#' env (an environment that provides access to the combobox choice).
#'
#' @param varname          a name for the parameter that will be controlled by a combobox
#' @param choices          the choices the user can select, a list of strings.
#' @param init             the initial choice on start. Optional, default is "".
#' @param label            a short description (max. 15 characters) of the controlled parameter. Optional, default is varname.
#' @param tip              a possibly longer description, that will (someday, currently broken) 
#'                         be displayed as a tip when the mouse is moved over the variable name.
#'
#' @return function(tkparent, env)
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
combobox <- function(varname, choices, init=NULL, label=NULL, tip="", varFunc=NULL) {
  # check arguments
  if(!is.character(varname)) stop("invalid varname argument, string expected.")
  if(!(is.character(choices) && is.vector(choices)) && !is.function(choices)) stop("invalid choices argument, a vector of strings or function expected.")
  if(is.null(init)) init <- ""
  if(!is.character(init)) stop("invalid init argument, string or NULL expected.")
  if(!is.character(tip)) stop("invalid tip argument, string expected.")
  if(is.null(label)) label <- varname

  # create combobox builder
  buildWidget <- function(tab, env) {
    result = tk2frame(tab)
    v <- tclVar(init) 
    w <- tk2label(result, text=label, tip=tip,  width=15, justify="right")
    tkpack(w, side="left", padx=5)
    # todo option if exact match is needed or not
    w <- tk2combobox(result, textvariable=v)
    if(is.function(choices))
      tkconfigure(w, postcommand=function() tkconfigure(w, values=choices(env)))
    else
      tkconfigure(w, values=choices)
    # todo add autocomplete, see http://wiki.tcl.tk/15780
    # autoComplete <- function(...) {
      # args <- list(...)
      # key <- args[[1]]
    # }  
    tkpack(w, side="left", padx=5)
    assign(sprintf("%s.getstate", varname), function() tclvalue(v), envir=env)
    assign(sprintf("%s.setstate", varname), function(newVal) tclvalue(v) <- newVal, envir=env)
    if(is.null(varFunc))
      assign(varname, function() tclvalue(v), envir=env)
    else
      assign(varname, function() varFunc(varname, env), envir=env)
    return(result)
  }
  return(buildWidget)
}

#' Creates a function that creates a entry widget.
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
entry <- function(varname, choices, init="", label=NULL, tip="") {
  # check arguments
  if(!is.character(varname)) stop("invalid varname argument, string expected.")
  if(!is.character(tip)) stop("invalid tip argument, string expected.")
  if(is.null(label)) label <- varname

  # create entry builder
  buildWidget <- function(tab, env) {
    result = tk2frame(tab)
    v <- tclVar(init) 
    w <- tk2label(result, text=label, tip=tip,  width=15, justify="right")
    tkpack(w, side="left", padx=5, anchor="w")
    w <- tk2entry(result, textvariable=v)
    tkpack(w, side="left", padx=5, fill="x")
    assign(varname, function() tclvalue(v), envir=env)
    return(result)
  }
  return(buildWidget)
}

#' Creates a function that creates a file name selector widget.
#'
#' The created function takes two parameters, tkParent (the parent for the new entry widget) and
#' env (an environment that provides access to the entry choice).
#'
#' @param varname          a name for the parameter that will store the filename
#' @param filter           specifies the allowed file types. A filter is specified in the form {{All files} {.*}}
#' @param init             the initial choice on start. Optional, default is the empty string
#' @param initDir         the directory which is presented on start. Default is "", current directory.
#' @param label            a short description (max. 15 characters) of the controlled parameter. Optional, default is varname.
#' @param tip              a possibly longer description, that will (someday, currently broken) 
#'                         be displayed as a tip when the mouse is moved over the variable name.
#'
#' @return function(tkparent, env)
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
fileName <- function (varname, filter = "{{All files} {.*}}", init="", initDir="", label=NULL, tip="") {
    # check arguments
    if(!is.character(varname)) stop("invalid varname argument, string expected.")
    if(!(is.null(filter) || is.character(filter))) stop("invalid filter argument, NULL or string or vector of strings expected.")
    if(!is.character(init)) stop("invalid init argument, string expected.")
    if(!is.character(tip)) stop("invalid tip argument, string expected.")
    if(!(is.null(label) || is.character(label))) stop("invalid label argument, NULL or string or vector of strings expected.")
    if(is.null(label)) label <- varname
    
    buildWidget <- function(tkParent, env) {
      resultFrame <- tk2frame(tkParent)
      var <- tclVar(init)
      w <- tk2label(resultFrame, text=varname, tip=tip,  width=15, justify="right")
      tkpack(w, side="left", padx=5)
      te <- tk2entry(resultFrame, textvariable=var)
      tkpack(te, side="left", padx=5, fill="x", expand=1)
      cmd <- function() {
        if(nchar(initDir)>0) {oldDir <- getwd(); setwd(initDir)}
        tempstr <- tclvalue(tkgetOpenFile(title=paste("Choose", label), filetypes=filter))
        if(nchar(initDir)>0) setwd(oldDir)
        if (nchar(tempstr)>0) {
          fullPath <- strsplit(tempstr, "/")[[1]]
          assign(paste(varname, "dir", sep="."), fullPath[1:length(fullPath)-1], envir=env)
          tkdelete(te, 0, "end")
          tkinsert(te, "end", fullPath[[length(fullPath)]])
        }
      }
      but <- tk2button(resultFrame, text="...", width=3, command = cmd)
      tkpack(but, side="left", padx=5)
      fullPathFunc <- function() {
        path <- c(get(paste(varname,"dir", sep="."), envir=env), tclvalue(var))
        return(do.call("file.path", as.list(path)))
      }
      assign(paste(varname, "dir", sep="."), NULL, envir=env)
      assign(varname, fullPathFunc, envir=env)
      return(resultFrame)
    }
    return(buildWidget)
}

#' Creates a function that creates a checkbox widget.
#'
#' The created function takes two parameters, tkParent (the parent for the new checkbox widget) and
#' env (an environment that provides access to the checkbox choice).
#'
#' @param varname          a name for the parameter that will be controlled by the checkbox
#' @param init             the initial choice on start. Optional, default is the empty string
#' @param label            a short description (max. 15 characters) of the controlled parameter. Optional, default is varname.
#' @param tip              a possibly longer description, that will be displayed as a tip when the mouse is moved over the variable name.
#'
#' @return function(tkparent, env)
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
checkbox <- function(varname, init=TRUE, label=NULL, tip="") {
  # check arguments
  if(!is.character(varname)) stop("invalid varname argument, string expected.")
  if(!is.logical(init)) stop("invalid init argument, boolean expected.")
  if(!is.character(tip)) stop("invalid tip argument, string expected.")
  if(is.null(label)) label <- varname

  # create entry builder
  buildWidget <- function(tab, env) {
    v <- tclVar(init) 
    result = tkframe(tab)
    w <- tk2checkbutton(result, text=label, variable=v, tip=tip)
    tkpack(w, padx=5, pady=5, fill="y", anchor="nw")
    assign(varname, function() as.logical(as.integer(tclvalue(v))), envir=env)
    return(result)
  }
  return(buildWidget)
}
