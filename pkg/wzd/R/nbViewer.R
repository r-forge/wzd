tkevent.propagate <- function(w,e) {
  tkevent.generate(w, e)
  children <- as.character(tkwinfo("children", w))
  if(length(children)>0) lapply(children, function(c) tkevent.propagate(c,e))
}
    
#' Creates and opens a Tcl/Tk window with several tabs.
#'
#' @param title            the name of the window
#' @param displayLabels    a character vector describing the tab names
#' @param displayFunctions a vector of functions with signature (tab, env) 
#'                         where tab is the tk2notetab object and env the notebook's environment.
#'                         Each function should return a tk2frame object with all the widget for the given tab.
#' @param initTab          Character sequence denoting the notetab to be activated first.
#' @param modal            if TRUE, the window will be modal, i.e. the focus will not return to the R console until
#'                         the window is closed. Default is !interactive().
#'
#' @return used for its side effects (gui)
#' @references 
#' \url{http://www.gapminder.org}
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
tabView <- function(title, displayLabels, displayFunctions, initTab="", modal=NULL) {
  if(!require(tcltk2)) stop("This function requires the package tcltk2.")
  if(!require(tcltk)) stop("This function requires the package tcltk.")
  
  # param checks
  if(length(displayLabels)!=length(displayFunctions)) stop("displayLabels and displayFunctions need to be vectors of the same length.")
  if(!(is.character(displayLabels)) || !(is.vector(displayLabels))) stop("displayLabels must be a vector of strings.")
  if(!(is.vector(displayFunctions)) || !all(sapply(displayFunctions, is.function))) stop("displayFunctions must be a vector of functions with one argument.")
  if((initTab!="") && is.na(match(initTab, displayLabels))) stop("initTab must appear in the displayLabels vector.")
  if(initTab=="") initTab <- displayLabels[[1]]
  if(is.null(modal)) modal <- !interactive()

  # toplevel
  tclServiceMode(FALSE) # don't display until complete
  root <- tktoplevel()
  tkwm.title(root, title)
  tkconfigure(root, width=800, height=600)

  # environment
  env <- new.env()
  v <- rep(TRUE, length(displayLabels))
  assign("needsUpdateData", v, envir=env)

  # navi bar
  navibar <- tk2frame(root)
  backButton <- tk2button(navibar, text="<--", width=5)
  tkpack(backButton, side="left", anchor="nw", padx=5, pady=5)
  forwButton <- tk2button(navibar, text="-->", width=5)
  tkpack(forwButton, side="left", anchor="nw", padx=5, pady=5)
  tkpack(tk2separator(navibar, orientation="vertical"), side="left", fill="y", padx=7, pady=5)
  updateButton <- tk2button(navibar, text="Update", width=8)
  tkpack(updateButton, side="left", anchor="nw", padx=5, pady=5)
  tkpack(tk2separator(navibar, orientation="vertical"), side="left", fill="y", padx=7, pady=5)
  bookmButton <- tk2button(navibar, text="Bookmark", width=8)
  tkpack(bookmButton, side="left", anchor="nw", padx=5, pady=5)
  bookmVar <- tclVar()
  bookmCombi <- tk2combobox(navibar, textvariable=bookmVar)
  tkpack(bookmCombi, side="left", anchor="nw", padx=5, pady=5, fill="x", expand=1)
  tkpack(navibar, expand=0, fill="x")

  # notebook
  nb <- tk2notebook(root, tabs=displayLabels)  
  tkpack(nb, fill="both", expand=1)
  tabRight <- function(...) {
    ntab <- as.integer(tkindex(nb, "end"))
    curr <- as.integer(tkindex(nb, tkselect(nb)))
    tkselect(nb, (curr+1) %% ntab)    
  }
  tabLeft <- function(...) {
    ntab <- as.integer(tkindex(nb, "end"))
    curr <- as.integer(tkindex(nb, tkselect(nb)))
    tkselect(nb, (curr-1) %% ntab)    
  }
  tkbind(root, "<Control-KeyPress-Next>", tabRight) # bind to nb does not work
  tkbind(root, "<Control-KeyPress-Prior>", tabLeft) 
  tkbind(nb, "<<NotebookTabChanged>>", function() print("tab changed event"))
  
  # lazy update
  needsUpdate <- function() {
    curr <- as.integer(tkindex(nb, tkselect(nb)))+1 # tk starts at 0
    return(get("needsUpdateData", envir=env)[[curr]])
  }
  finishedUpdate <- function() {
    curr <- as.integer(tkindex(nb, tkselect(nb)))+1 # tk starts at 0
    eval(parse(text=sprintf("needsUpdateData[[%d]] <- FALSE", curr)), envir=env)
  }
  updateCommand <- function() {
    v <- rep(TRUE, length(displayLabels))
    assign("needsUpdateData", v, envir=env)
    curTab <- as.integer(tkindex(nb, tkselect(nb)))+1 # tk starts at 0
    curTabId <- paste(nb$ID, curTab, sep=".")
    tkevent.propagate(curTabId, "<<NotebookUpdated>>")
  }
  notetabChangedCommand <- function() {
    curTab <- as.integer(tkindex(nb, tkselect(nb)))+1 # tk starts at 0
    curTabId <- paste(nb$ID, curTab, sep=".")
    tkevent.propagate(curTabId, "<<NotebookUpdated>>")
  }    
  tkconfigure(updateButton, command=updateCommand)
  tkbind(root, "<Control-KeyPress-r>", updateCommand)
  tkbind(nb, "<<NotebookTabChanged>>", notetabChangedCommand)
  assign("needsUpdate", needsUpdate, envir=env)
  assign("finishedUpdate", finishedUpdate, envir=env)

  # notebook tabs  
  for (i in 1:length(displayLabels)) {
    tab <- tk2notetab(nb, displayLabels[[i]])
    tkconfigure(tab, takefocus=0)
    tkpack(displayFunctions[[i]](tab, env), fill="both", expand=1)
  }
  
  # set actual size as minimum
  width <- tkwinfo("width", root)
  height <- tkwinfo("height", root)
  tkwm.minsize(root, width, height)
  
  # start
  tclServiceMode(TRUE)
  ico <- tk2ico.load(file.path(R.home(), "bin", "R.exe"), res = "R")
  tk2ico.set(root, ico)
  tk2ico.destroy(ico)
  tkgrab.set(root)
  tkfocus(root)
  tk2notetab.select(nb, initTab)
  if(modal) tkwait.window(root)
}

#' Creates and opens a Tcl/Tk window without tabs.
#'
#' @param title            the name of the window
#' @param buildWidget      a function(tkParent, env) that returns a tk widget
#' @param modal            if TRUE, the window will be modal, i.e. the focus will not return to the R console until
#'                         the window is closed. Default is !interactive().
#'
#' @return used for its side effects (gui)
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
widgetView <- function(title, buildWidget, modal=NULL) {
  if(!require(tcltk2)) stop("This function requires the package tcltk2.")
  if(!require(tcltk)) stop("This function requires the package tcltk.")
  
  # param checks
  if(!is.character(title)) stop("invalid title argument - character required.")
  if(!is.function(buildWidget)|| (length(formals(buildWidget))!=2)) stop("invalid buildWidget argument - function(tkParent, env) required.")
  if(is.null(modal)) modal <- !interactive()
  
  # toplevel
  tclServiceMode(FALSE) # don't display until complete
  root <- tktoplevel()
  tkwm.title(root, title)
  tkconfigure(root, width=800, height=600)
  
  # update button
  updateCommand <- function() {
    assign("needsUpdateData", TRUE, envir=env)
    tkevent.propagate(root, "<<NotebookUpdated>>")
  }
  updateButton <- tk2button(root, text="Update", command=updateCommand, takefocus=0)
  tkpack(updateButton, anchor="nw", padx=5, pady=5)
  tkbind(root, "<Control-KeyPress-r>", updateCommand)

  # environment, function for lazy update
  env <- new.env()
  #env <- .GlobalEnv
  needsUpdate <- function() return(get("needsUpdateData", envir=env))
  finishedUpdate <- function() eval(parse(text="needsUpdateData <- FALSE"), envir=env)
  assign("needsUpdateData", TRUE, envir=env)
  assign("needsUpdate", needsUpdate, envir=env)
  assign("finishedUpdate", finishedUpdate, envir=env)

  # pack and run
  tkpack(buildWidget(root,env), fill="both", expand=1)
  tclServiceMode(TRUE)
  ico <- tk2ico.load(file.path(R.home(), "bin", "R.exe"), res = "R")
  tk2ico.set(root, ico)
  tk2ico.destroy(ico)
  tkgrab.set(root)
  tkfocus(root)
  if(modal) tkwait.window(root)
}
