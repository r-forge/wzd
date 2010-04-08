#' Driver for Sweave() which outputs MWiki formatted text.
#'
#' The code is, with only very few adaptions, taken from the package ascii by David Hajage.
#'
#' @param None
#' @return a list object suitable as driver parameter for Sweave.
#' @export
#' @references
#' \url{http://wagezudenken.blogspot.com/2009/12/r-workflow-test.html}
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
RweaveMWiki <- function () 
{
    list(setup = RweaveMWikiSetup, runcode = RweaveMWikiRuncode, 
        writedoc = RweaveMWikiWritedoc, finish = RweaveMWikiFinish, 
        checkopts = RweaveMWikiOptions)
}

#' Internal function for RweaveMWiki.
#'
#' Sets the options for the RweaveMWiki driver.
#'
#' @param file input file name
#' @param syntax  syntax object
#' @param output output file name (NULL) 
#' @param quiet print some information (FALSE)
#' @param debug print more information (FALSE)
#' @param ... prefix, prefix.string, engine, print, eval, fig, ext, png, jpg, pdf, eps, width, height, term, echo, keep.source, results, split, strip.white, include, pdf.version, pdf.encoding, concordance, expand
#' @return a list with components output, debug, quiet, syntax, options, chunkout, srclines and srcfile
RweaveMWikiSetup <- function (file, syntax, output=NULL, quiet=FALSE, debug=FALSE, ...) 
{
    dots <- list(...)
    if (is.null(output)) {
        prefix.string <- basename(sub(".Rmwiki", "", file))
        output <- paste(prefix.string, "mwiki", sep = ".")
    }
    else {
        prefix.string <- basename(sub("\\.mwiki$", "", output))
    }
    if (!quiet) 
        cat("Writing to file ", output, "\n", "Processing code chunks ...\n", sep = "")
    output <- file(output, open = "w+")
    options <- list(prefix = TRUE, prefix.string = prefix.string, 
        engine = "R", print = FALSE, eval = TRUE, fig = FALSE, 
        ext = "jpg", png = FALSE, jpg = TRUE, pdf = FALSE, eps = FALSE, 
        width = 4, height = 3, term = TRUE, echo = TRUE, res=100,
        keep.source = FALSE, results = "verbatim", split = FALSE, 
        strip.white = TRUE, include = TRUE, pdf.version = grDevices::pdf.options()$version, 
        pdf.encoding = grDevices::pdf.options()$encoding, concordance = FALSE, 
        expand = TRUE)
    options[names(dots)] <- dots  
    list(output = output, debug = debug, quiet = quiet, syntax = syntax, 
        options = options, chunkout = list(), srclines = integer(0L), 
        srcfile = srcfile(file))
}

#' Internal function for RweaveMWiki.
#'
#' evaluates an R expression
#' @param expr R expression as string
#' @param options list of options. Components print, eval and term are read
#' @return result of eval(expr)
RweaveEvalWithOpt <- function (expr, options) 
{
    if (options$eval) {
        res <- try(withVisible(eval(expr, .GlobalEnv)), silent = TRUE)
        if (inherits(res, "try-error")) 
            return(res)
        if (options$print | (options$term & res$visible)) 
            print(res$value)
    }
    return(res)
}


#' Internal function for RweaveMWiki.
#'
#' runs one chunk of R code
#' @param object the object
#' @param chunk the chunk
#' @param options the options
#' @return object
RweaveMWikiRuncode <- function (object, chunk, options) 
{
    if (!(options$engine %in% c("R", "S"))) {
        return(object)
    }
    if (!object$quiet) {
        cat(formatC(options$chunknr, width = 2), ":")
        if (options$echo) 
            cat(" echo")
        if (options$keep.source) 
            cat(" keep.source")
        if (options$eval) {
            if (options$print) 
                cat(" print")
            if (options$term) 
                cat(" term")
            cat("", options$results)
            if (options$fig) {
                if (options$png) 
                  cat(" png")
                if (options$jpg) 
                  cat(" jpg")
                if (options$eps) 
                  cat(" eps")
                if (options$pdf) 
                  cat(" pdf")
            }
        }
        if (!is.null(options$label)) 
            cat(" (label=", options$label, ")", sep = "")
        cat("\n")
    }
    chunkprefix <- RweaveChunkPrefix(options)
    if (options$split) {
        chunkout <- object$chunkout[chunkprefix][[1L]]
        if (is.null(chunkout)) {
            chunkout <- file(paste(chunkprefix, "txt", sep = "."), 
                "w")
            if (!is.null(options$label)) 
                object$chunkout[[chunkprefix]] <- chunkout
        }
    }
    else chunkout <- object$output
    saveopts <- options(keep.source = options$keep.source)
    on.exit(options(saveopts))
    SweaveHooks(options, run = TRUE)
    chunkexps <- try(parse(text = chunk), silent = TRUE)
    RweaveTryStop(chunkexps, options)
    openSinput <- FALSE
    openSchunk <- FALSE
    if (length(chunkexps) == 0L) 
        return(object)
    srclines <- attr(chunk, "srclines")
    linesout <- integer(0L)
    srcline <- srclines[1L]
    srcrefs <- attr(chunkexps, "srcref")
    if (options$expand) 
        lastshown <- 0L
    else lastshown <- srcline - 1L
    thisline <- 0
    for (nce in 1L:length(chunkexps)) {
        ce <- chunkexps[[nce]]
        if (nce <= length(srcrefs) && !is.null(srcref <- srcrefs[[nce]])) {
            if (options$expand) {
                srcfile <- attr(srcref, "srcfile")
                showfrom <- srcref[1L]
                showto <- srcref[3L]
            }
            else {
                srcfile <- object$srcfile
                showfrom <- srclines[srcref[1L]]
                showto <- srclines[srcref[3L]]
            }
            dce <- getSrcLines(srcfile, lastshown + 1, showto)
            leading <- showfrom - lastshown
            lastshown <- showto
            srcline <- srclines[srcref[3L]]
            while (length(dce) && length(grep("^[[:blank:]]*$", 
                dce[1L]))) {
                dce <- dce[-1L]
                leading <- leading - 1L
            }
        }
        else {
            dce <- deparse(ce, width.cutoff = 0.75 * getOption("width"))
            leading <- 1L
        }
        if (object$debug) 
            cat("\nRnw> working on: ", paste(dce, collapse = "\n+  "), "\n")
        if (options$echo && length(dce)) {
            if (!openSinput) {
                if (object$debug) cat("Rnw> options$echo, !openSinput, before !openSchunk\n", append = TRUE)
                if (!openSchunk) {
                  if (object$debug) cat("Rnw> options$echo, !openSinput, !openSchunk\n", append = TRUE)
                  cat("===>as-is\n", file = chunkout, append = TRUE)
                  linesout[thisline + 1] <- srcline
                  thisline <- thisline + 1
                  openSchunk <- TRUE
                }
                if (object$debug) cat("Rnw> options$echo, !openSinput, after !openSchunk\n", append = TRUE)
                openSinput <- TRUE
            }
            if (object$debug) cat("Rnw> options$echo, before prompt\n", append = TRUE)
            cat("", paste(getOption("prompt"), dce[1L:leading], 
                sep = "", collapse = "\n"), file = chunkout, 
                append = TRUE, sep = "")
            if (length(dce) > leading) 
                cat("\n", paste(getOption("continue"), dce[-(1L:leading)], 
                  sep = "", collapse = "\n"), file = chunkout, 
                  append = TRUE, sep = "")
            linesout[thisline + 1L:length(dce)] <- srcline
            thisline <- thisline + length(dce)
        }
        tmpcon <- file()
        sink(file = tmpcon)
        err <- NULL
        if (options$eval) 
            err <- RweaveEvalWithOpt(ce, options)

        cat("")
        sink()
        output <- readLines(tmpcon)
        close(tmpcon)
        if (length(output) == 1L & output[1L] == "") 
            output <- NULL
        RweaveTryStop(err, options)
        if (object$debug) 
            cat(paste(output, collapse = "\n"))
        if (length(output) == 0 | options$results == "hide") 
            cat("\n", file = chunkout, append = TRUE)
        if (length(output) & (options$results != "hide")) {
            addabreak <- ""
            if (openSinput) {
                if (object$debug) cat("Rnw> options$results != hide, openSinput\n", append = TRUE)
                cat("\n", file = chunkout, append = TRUE)
                linesout[thisline + 1L:2L] <- srcline
                thisline <- thisline + 2L
                openSinput <- FALSE
            }
            if (options$results == "verbatim") {
                if (object$debug) cat("Rnw> options$results==verbatim, before !openSchunk\n", append = TRUE)
                if (!openSchunk) {
                  if (object$debug) cat("Rnw> options$results==verbatim, in !openSchunk\n", append = TRUE)
                  cat("===>as-is\n", file = chunkout, append = TRUE)
                  cat("", file = chunkout, append = TRUE)
                  linesout[thisline + 1L] <- srcline
                  thisline <- thisline + 1L
                  openSchunk <- TRUE
                }
                if (object$debug) cat("Rnw> options$results==verbatim, after !openSchunk\n", append = TRUE)
                cat("", file = chunkout, append = TRUE)
                linesout[thisline + 1L] <- srcline
                thisline <- thisline + 1L
            }
            if (options$results == "ascii") {
                if (openSinput) {
                  cat("", file = chunkout, append = TRUE)
                  linesout[thisline + 1L] <- srcline
                  thisline <- thisline + 1L
                  openSchunk <- TRUE
                }
                if (openSchunk) {
                  cat("", file = chunkout, append = TRUE)
                  linesout[thisline + 1L] <- srcline
                  thisline <- thisline + 1L
                  openSchunk <- FALSE
                }
                addabreak <- "\n"
            }
            output <- paste(output, collapse = "\n")
            if (options$strip.white %in% c("all", "true")) {
                output <- sub("^[[:space:]]*\n", "", output)
                output <- sub("\n[[:space:]]*$", "", output)
                if (options$strip.white == "all") 
                  output <- sub("\n[[:space:]]*\n", "\n", output)
            }
            if (object$debug) cat("Rnw> before output\n", append = TRUE)
            cat(output, addabreak, file = chunkout, append = TRUE)
            if (object$debug) cat("\nRnw> after output\n", append = TRUE)
            count <- sum(strsplit(output, NULL)[[1L]] == "\n")
            if (count > 0L) {
                linesout[thisline + 1L:count] <- srcline
                thisline <- thisline + count
            }
            remove(output)
            if (options$results == "verbatim") {
                if (object$debug) cat("Rnw> results=verbatim, after output\n", append = TRUE)
                cat("\n", file = chunkout, append = TRUE)
                linesout[thisline + 1L:2] <- srcline
                thisline <- thisline + 2L
            }
        }
    }
    if (openSinput) {
        if (object$debug) cat("Rnw> openSinput\n", append = TRUE)
        linesout[thisline + 1L:2L] <- srcline
        thisline <- thisline + 2L
    }
    if (openSchunk) {
        if ((options$echo) || (options$results == "verbatim")) cat("<===\n", file = chunkout, append = TRUE)
        if (object$debug) cat("Rnw> openSchunk\n", append = TRUE)
        linesout[thisline + 1L] <- srcline
        thisline <- thisline + 1L
    }
    if (is.null(options$label) & options$split) 
        close(chunkout)
    if (options$fig && options$eval) {
        if (options$jpg) {
            if (object$debug) cat("Rnw> fig, eval\n", append = TRUE)
            jpeg(file = paste(chunkprefix, "jpg", sep = "."), 
                width = options$width, height = options$height, 
                quality = 100, units = "in", res = options$res)
            err <- try({
                SweaveHooks(options, run = TRUE)
                eval(chunkexps, envir = .GlobalEnv)
            })
            dev.off()
            cat(paste("[-", paste(chunkprefix, "jpg", sep = "."), "-]\n"), file = chunkout, append = TRUE)
            if (inherits(err, "try-error")) 
                stop(err)
        }
    }
    if (object$debug) cat("Rnw> end of runcode\n", append = TRUE)
    object$linesout <- c(object$linesout, linesout)
    return(object)
}

#' Internal function for RweaveMWiki.
#'
#' writes out a evaluated code chunk.
#' @param object the object
#' @param chunk the chunk
#' @return object
RweaveMWikiWritedoc <- function (object, chunk) 
{
   linesout <- attr(chunk, "srclines")
    while (length(pos <- grep(object$syntax$docexpr, chunk))) {
        cmdloc <- regexpr(object$syntax$docexpr, chunk[pos[1L]])
        cmd <- substr(chunk[pos[1L]], cmdloc, cmdloc + attr(cmdloc, 
            "match.length") - 1L)
        cmd <- sub(object$syntax$docexpr, "\\1", cmd)
        if (object$options$eval) {
            val <- as.character(eval(parse(text = cmd), envir = .GlobalEnv))
            if (length(val) == 0L) 
                val <- ""
        }
        else val <- paste("\\\\verb{<<", cmd, ">>{", sep = "")
        chunk[pos[1L]] <- sub(object$syntax$docexpr, val, chunk[pos[1L]])
    }
    while (length(pos <- grep(object$syntax$docopt, chunk))) {
        opts <- sub(paste(".*", object$syntax$docopt, ".*", sep = ""), 
            "\\1", chunk[pos[1L]])
        object$options <- utils:::SweaveParseOptions(opts, object$options, 
            RweaveMWikiOptions)
        chunk[pos[1L]] <- sub(object$syntax$docopt, "", chunk[pos[1L]])
    }
    cat(chunk, sep = "\n", file = object$output, append = TRUE)
    object$linesout <- c(object$linesout, linesout)
    return(object)
}

#' Internal function for RweaveMWiki.
#'
#' Hook called at the end of a Sweave call.
#' @param object the object
#' @param error default FALSE
#' @return object
RweaveMWikiFinish <- function (object, error = FALSE) 
{
    if (!object$quiet && !error) 
        cat(paste("file ", summary(object$output)$description), 
            "is completed", "\n")
    close(object$output)
    if (length(object$chunkout) > 0) {
        for (con in object$chunkout) close(con)
    }
}

#' Internal function for RweaveMWiki.
#'
#' Check the sanity of the options. Stops on invalid options.
#' @param options the options
#' @return options
RweaveMWikiOptions <- function (options) 
{
    c2l <- function(x) {
        if (is.null(x)) 
            return(FALSE)
        else return(as.logical(toupper(as.character(x))))
    }
    NUMOPTS <- c("width", "height", "res")
    NOLOGOPTS <- c(NUMOPTS, "ext", "results", "prefix.string", "engine", 
        "label", "align", "caption", "border", "height", "width", 
        "HTMLheight", "pdf.version", "pdf.encoding", "bg", "pointsize")
    for (opt in names(options)) {
        if (!(opt %in% NOLOGOPTS)) {
            oldval <- options[[opt]]
            if (!is.logical(options[[opt]])) {
                options[[opt]] <- c2l(options[[opt]])
            }
            if (is.na(options[[opt]])) 
                stop(paste("invalid value for", opt, ":", oldval))
        }
        else if (opt %in% NUMOPTS) {
            options[[opt]] <- as.numeric(options[[opt]])
        }
    }
    options$results <- match.arg(options$results, c("verbatim", "hide"))
    options
}

