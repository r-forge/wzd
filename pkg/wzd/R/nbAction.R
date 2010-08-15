#' binds a function to the <<NotebookUpdated>> event
#'
#' @param widget           a Tk widget
#' @param env              the notebook's environment
#' @param func             the function to bind to
#'
#' @return NULL
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
tkbind.updater <- function(widget, env, func) {
  updateFunc <- function() if(get("needsUpdate", envir=env)()) {
    func()
    get("finishedUpdate", envir=env)() # mark as updated
  }
  tkbind(widget, "<<NotebookUpdated>>", updateFunc)
}
  
#' Explore a text producing function by graphically manipulating its parameters.
#'
#' @param f                the function to explore
#' @param paramNames       a character vector describing naming the parameters
#' @param paramDisplays    a vector of functions which describes the graphical control of each parameter
#'
#' @return a function(tkParent, env) for use with tabView or widgetView
#' @references 
#' \url{http://www.gapminder.org}
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
textManipulate <- function(f, paramNames, paramDisplays) {
  # param checks
  if(!is.function(f)) stop("invalid f argument, function expected.")
  if(length(paramNames)!=length(paramDisplays)) stop("paramNames and paramDisplays need to be vectors of the same length.")
  if(!(is.character(paramNames)) || !(is.vector(paramNames))) stop("invalid paramNames argument, vector of strings expected.")
  if(!(is.vector(paramDisplays)) || !all(sapply(paramDisplays, is.function))) stop("invalid paramDisplays argument, vector of functions expected.")

  buildWidget <- function(tkParent, env) {
    tk2font.set("cour10", list(family="Courier New", size=10))
    
    # the overall widget we build
    resultFrame <- tk2frame(tkParent)
    
    # widget for the R output 
    outputFrame <- tk2frame(resultFrame)
    yscr <- tkscrollbar(outputFrame, repeatinterval=5, command=function(...) tkyview(outputWin, ...))
    outputWin <- tk2text(outputFrame, 
     takefocus=0, state="disabled", wrap="none", font="cour10",     
     yscrollcommand = function(...) tkset(yscr, ...)
    )
    tkpack(outputWin, side="left", fill="both", expand=1)
    tkpack(yscr, side="left", fill="y", expand=0)    
    
    # callback for displaying the output
    dumpOutput <- function() {
      con <- textConnection("unused", "w")
      on.exit({sink();close(con)})
      sink(con)
      paramValues <- lapply(paramNames, function(p) eval(call(p), envir=env))
      names(paramValues) <- paramNames
      do.call(f, paramValues)
      output <- textConnectionValue(con)
      output <- paste(output, collapse="\n")
      tkconfigure(outputWin, state="normal")
      tkdelete(outputWin, "0.0", "end")
      tkinsert(outputWin, "0.0", output)
      tksee(outputWin, "end")
      tkconfigure(outputWin, state="disabled")
    }

    # widget for the controls
    ctrlFrame <- tk2frame(resultFrame)
    for (i in 1:length(paramDisplays)) {
      widget <- paramDisplays[[i]](ctrlFrame, env)
      tkpack(widget, side="top", fill="x")
    }
        
    # laying it all out 
    tkpack(ctrlFrame, side="left", anchor="ne", pady=5)
    tkpack(outputFrame, side="right", anchor="nw", fill="both", expand=1, pady=5, padx=5)
    tkbind.updater(outputFrame, env, dumpOutput)
        
    return(resultFrame)
  }
  return(buildWidget) 
}

#' Explore a plot producing function by graphically manipulating its parameters.
#'
#' @param f                the function to explore
#' @param paramNames       a character vector describing naming the parameters
#' @param paramDisplays    a vector of functions which describes the graphical control of each parameter
#'
#' @return a function(tkParent, env) for use with tabView or widgetView
#' @references 
#' \url{http://www.gapminder.org}
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
plotManipulate <- function(f, paramNames, paramDisplays) {
  if(!require(tkrplot)) stop("Could not load required library tkrplot.")
  if(!is.function(f)) stop("invalid f argument, function expected.")
  if(length(paramNames)!=length(paramDisplays)) stop("paramNames and paramDisplays need to be vectors of the same length.")
  if(!(is.character(paramNames)) || !(is.vector(paramNames))) stop("invalid paramNames argument, vector of strings expected.")
  if(!(is.vector(paramDisplays)) || !all(sapply(paramDisplays, is.function))) stop("invalid paramDisplays argument, vector of functions expected.")

  buildWidget <- function(tkParent, env) {
    # the overall widget we build
    resultFrame <- tk2frame(tkParent)
      
    # widget for the controls
    ctrlFrame <- tk2frame(resultFrame)
    for (i in 1:length(paramDisplays)) {
      widget <- paramDisplays[[i]](ctrlFrame, env)
      tkpack(widget, side="top", fill="x")
    }
    
    # unique names for height/weight parameters and convenience functions
    if(!exists(".plotManipulateCounter", envir=env))
      assign(".plotManipulateCounter", 1, envir=env)
    else {
      count <- get(".plotManipulateCounter", envir=env)
      assign(".plotManipulateCounter", count+1, envir=env)
    }
    count <- get(".plotManipulateCounter", envir=env)
    heightVar <- sprintf(".height.%d", count)
    widthVar <- sprintf(".width.%d", count)
    cm2inch <- function(varName) as.numeric(eval(call(varName), envir=env))*0.3937
    
    # pack controls
    tkpack(tk2separator(ctrlFrame, orientation="horizontal"), side="top", fill="x", pady=7, padx=5)
    widget <- slider(heightVar, from=4, to=16, digits=1, label="height (cm)", tip="Select the size of the output")(ctrlFrame, env)
    tkpack(widget, side="top", fill="x")
    widget <- slider(widthVar, from=4, to=21, digits=1, label="width (cm)", tip="Select the size of the output")(ctrlFrame, env)
    tkpack(widget, side="top", fill="x")
    tkpack(ctrlFrame, side="left", anchor="ne", pady=5)
    
    # pack output widget
    createPlot <-function() {
      paramValues <- lapply(paramNames, function(p) eval(call(p), envir=env))
      names(paramValues) <- paramNames
      do.call(f, paramValues)
    }
    img <-tkrplot(resultFrame, fun=createPlot, hscale=cm2inch(widthVar)/3, vscale=cm2inch(heightVar) / 3)
    updatePlot <- function() {
      tkrreplot(img, , hscale=cm2inch(widthVar)/3, vscale=cm2inch(heightVar) / 3)
      # keep the focus
      tkRoot <- tcl("winfo", "toplevel", img)
      tcl("focus", "-force", tkRoot)
    }
    tkbind.updater(img, env, updatePlot)
    tkpack(img, side="right", anchor="nw", fill="both", expand=1, pady=5, padx=5)
    
    return(resultFrame)
  }
  return(buildWidget) 
}
