nbDemo <- function() {
  tabView("tabView Demo", 
     c("Random Normal", "Contingence Table", "SimplePlot", "File", "Memory", "Test"), 
     c(rnormWidget(), tableWidget(), plotWidget(), fileWidget(), memoryWidget(), notImplementedYet)
   )
}

# todo write doc
wDemo <- function(w=plotWidget) widgetView(deparse(substitute(w)), w())

fileWidget <- function() {
  myF <- function(fname, ok) {print(fname);print(ok)}
  textManipulate(
    myF,
    c("fname", "ok"),
    c(fileName("fname", initDir="~", tip="Choose text file", label="text file"),
      checkbox("ok", init=TRUE, tip="Check this if you're OK", label="ok?")
    )
  )
}

plotWidget <- function() {
  myF <- function(bb) plot(1:20, (1:20)^bb)
  plotManipulate(
    myF,
    c("bb"),
    c(slider("bb", from=0.05, to=2.00, digits=2, tip="Choose exponent"))
  )
}
  
tableWidget <- function() {

  # manipulate the table function
  myF <- function(dataset, rows, columns, useNA) {
    #print(summary(dataset))
    # return(NULL)
    if(is.na(match(rows, names(dataset)))) print("Invalid rows argument, not in dataset.")
      else if(is.na(match(rows, names(dataset)))) print("Invalid column argument, not in dataset.")
        else print(with(dataset, do.call("table", list(eval(parse(text=rows)), eval(parse(text=columns)), useNA=useNA))))
  }
  
  # only consider data.frame with at least two factors
  filterFunc <- function(ds) {
      if(!is.data.frame(ds)) return(FALSE)
      cols <- colnames(ds)
      cols <- cols[sapply(cols, function(c) is.factor(ds[,c]))]
      return(length(cols)>2)
  }

  textManipulate(
      myF, 
      c("dataset", "rows", "columns", "useNA"), 
      c(
        installedDataSetCombo("dataset", filter=filterFunc, tip="Which dataset do you want to look at?"),
        fieldFromDataset("rows", "dataset", factorsOnly=TRUE, tip="Which field will appear as rows?"),
        fieldFromDataset("columns", "dataset", factorsOnly=TRUE, tip="Which field will appear as columns?"),
        combobox("useNA", c("no", "ifany", "always"), init="ifany", tip="Whether to include extra ‘NA’ levels in the table.")
      )
  )
}

rnormWidget <- function() {
  myF <- function(n, mean, sd) {
    print(sprintf("Creating %d samples, normally distributed with mean=%f and standard deviation=%f", n, mean, sd))
    sample <- rnorm(n,mean, sd)
    print(sample)
    print(summary(sample))
  }
    
  textManipulate(
      myF, 
      c("n", "mean", "sd"), 
      c(
        slider("n", 1, 2000, digits=0, tip="The size of the sample."), 
        slider("mean", -10, 10, digits=1, tip="The expected value of the underlying random variable."), 
        slider("sd", 0, 5, digits=2, tip="The standard deviation of the underlying random variable.")
      )
  )
}

makeCharMat <- function(x) {
  mat <- matrix(unlist(x), nrow = nrow(as.matrix(x)))
  dm <- dim(mat)
  hasRownames <- length(rn <- rownames(x)) > 0
  hasColnames <- length(cn <- colnames(x)) > 0
  if (!hasRownames) 
      rn <- paste("[", 1:nrow(x), ",]", sep = "")
  if (!hasColnames) 
      cn <- paste("[,", 1:ncol(x), "]", sep = "")
  mat[] <- apply(unclass(mat), 2, format, justify = "right")
  mat <- rbind(cn, mat)
  mat <- cbind(c("", rn), mat)
  mat
}

fillTclArrayFromCharMat <- function(ta, cm) {
  for (j in 2:ncol(cm)) ta[[0, j - 1]] <- as.tclObj(cm[1, j], drop = TRUE)
  for (i in 2:nrow(cm)) for (j in 1:ncol(cm)) ta[[i - 1, j - 1]] <- as.tclObj(cm[i, j], drop = TRUE)
}

editDataSet <- function(tab, env) 
# x, title = "Matrix Editor", header = NULL, maxHeight = 600, maxWidth = 800, fontsize = 9, ...) 
{
# if (!is.tk()) 
    # stop("Package Tk is required but not loaded")
# if (!inherits(tclRequire("Tktable", warn = FALSE), "tclObj")) 
    # stop("Tcl package 'Tktable' must be installed first")
    .Tcl(paste("option add *Table.font {courier", 9, "bold}"))
    old <- options(scipen = 7)
    on.exit(options(old))
    tA <- tclArray()
#    cmat <- makeCharMat(x)
    cmat <- makeCharMat(loadDataSet(env))
    fillTclArrayFromCharMat(tA, cmat)
#    tt <- tktoplevel()
#    tkwm.title(tt, title)
    colwidths <- apply(cmat, 2, function(x) max(nchar(x)) + 1)
    nTableCols <- ncol(cmat)
    if ((moreWidth <- 60 - sum(colwidths)) > 0) {
        addEach <- moreWidth%/%length(colwidths)
        if (addEach < 5) 
            colwidths <- colwidths + addEach + 1
        else nTableCols <- nTableCols + ceiling(moreWidth/10)
    }
    result = tk2frame(tab)
    tktable <- tkwidget(result, "table", variable = tA, rows = nrow(cmat), 
        cols = nTableCols, titlerows = 1, titlecols = 1, selecttitle = 1, 
        anchor = "e", multiline = 0, selectmode = "extended", 
        rowseparator = dQuote("\n"), colseparator = dQuote("\t"), 
        background = "white", maxheight = 600, maxwidth = 800, 
        xscrollcommand = function(...) tkset(xscr, ...), 
        yscrollcommand = function(...) tkset(yscr, ...))
    xscr <- tkscrollbar(result, orient = "horizontal", command = function(...) tkxview(tktable, ...))
    yscr <- tkscrollbar(result, command = function(...) tkyview(tktable, ...))
    for (i in 1:ncol(cmat)) tcl(tktable, "width", i - 1, colwidths[i])
    string <- "bind Table <BackSpace> {\n\t\tset ::tk::table::Priv(junk) [%W icursor]\n    \tif {[string compare {} $::tk::table::Priv(junk)] && $::tk::table::Priv(junk)} {\n\t\t%W delete active [expr {$::tk::table::Priv(junk)-1}]\n    \t}}"
    .Tcl(string)
    activeRow <- function() as.numeric(tkindex(tktable, "active", "row"))
    activeCol <- function() as.numeric(tkindex(tktable, "active", "col"))
    undoEdits <- function() {
        ta <- tclArray()
        fillTclArrayFromCharMat(ta, cmat)
        assign("tA", ta, inherits = TRUE)
        tkconfigure(tktable, variable = tA)
    }
    insertRow <- function() {
        row <- activeRow()
        col <- activeCol()
        tkinsert(tktable, "rows", row, 1)
        newCell <- paste(row + 1, col, sep = ",")
        tkactivate(tktable, newCell)
        tksee(tktable, newCell)
    }
    insertCol <- function() {
        row <- activeRow()
        col <- activeCol()
        tkinsert(tktable, "cols", col, 1)
        newCell <- paste(row, col + 1, sep = ",")
        tkactivate(tktable, newCell)
        tksee(tktable, newCell)
    }
    deleteRow <- function() {
        if ((row <- activeRow()) != 0) 
            tkdelete(tktable, "rows", row, 1)
    }
    deleteCol <- function() {
        if ((col <- activeCol()) != 0) 
            tkdelete(tktable, "cols", col, 1)
    }
    copyRow <- function() {
        src <- activeRow()
        if (src != 0) {
            insertRow()
            dst <- activeRow()
            for (j in 0:(ncol(tA) - 1)) tA[[dst, j]] <- tA[[src, j]]
        }
    }
    copyCol <- function() {
        src <- activeCol()
        if (src != 0) {
            insertCol()
            dst <- activeCol()
            for (i in 0:(nrow(tA) - 1)) tA[[i, dst]] <- tA[[i, src]]
        }
    }
    undoEditsButton <- tkbutton(result, text = "Undo Edits", command = undoEdits)
    insertRowButton <- tkbutton(result, text = "Insert Row", command = insertRow)
    copyRowButton <- tkbutton(result, text = "Copy Row", command = copyRow)
    deleteRowButton <- tkbutton(result, text = "Delete Row", command = deleteRow)
    insertColButton <- tkbutton(result, text = "Insert Col", command = insertCol)
    copyColButton <- tkbutton(result, text = "Copy Col", command = copyCol)
    deleteColButton <- tkbutton(result, text = "Delete Col", command = deleteCol)
    # if (length(header) > 0) {
        # for (label in header) tkgrid(tklabel(tt, text = label), 
            # columnspan = 7, sticky = "nw")
    # }
    tkgrid(tktable, yscr, columnspan = 8)
    tkgrid.configure(tktable, sticky = "news")
    tkgrid.configure(yscr, sticky = "nsw")
    tkgrid(xscr, sticky = "new", columnspan = 8)
    tkgrid(insertRowButton, copyRowButton, deleteRowButton, sticky = "news")
    tkgrid(insertColButton, copyColButton, deleteColButton, "x", undoEditsButton, sticky = "news")
    tkgrid.columnconfigure(result, 3, weight = 1)
    tkgrid.rowconfigure(result, length(header), weight = 1)
    tkactivate(tktable, "0,0")
    tktag.configure(tktable, "active", background = "lightyellow2")
    tktag.configure(tktable, "title", state = "normal")
    return(result)
    # outMat <- matrix("", nrow = nrow(tA), ncol = ncol(tA))
    # for (i in 1:nrow(outMat)) {
        # for (j in 1:ncol(outMat)) {
            # val <- tA[[i - 1, j - 1]]
            # if (is.null(val)) 
                # val <- ""
            # else val <- tclvalue(val)
            # outMat[i, j] <- val
        # }
    # }
    # rn <- outMat[, 1][-1]
    # cn <- outMat[1, ][-1]
    # outMat <- outMat[-1, -1, drop = FALSE]
    # badRownames <- c(grep("\\[.*\\]", rn), (1:length(rn))[is.na(rn)])
    # if (length(badRownames) != length(rn)) {
        # rn[badRownames] <- ""
        # rownames(outMat) <- rn
    # }
    # badColnames <- c(grep("\\[.*\\]", cn), (1:length(cn))[is.na(cn)])
    # if (length(badColnames) != length(cn)) {
        # cn[badColnames] <- ""
        # colnames(outMat) <- cn
    # }
    # mode(outMat) <- mode(x)
    # Sys.sleep(0.1)
    # return(outMat)
}