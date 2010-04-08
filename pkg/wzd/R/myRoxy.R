myRoxy <-function (package.dir, roxygen.dir = NULL, copy.package = TRUE, 
    overwrite = TRUE, unlink.target = FALSE, use.Rd2 = TRUE) 
{
    if (is.null(roxygen.dir)) 
        roxygen.dir <- file.path(dirname(normalizePath(package.dir)), "pkg")
    print(paste("output to ", roxygen.dir))
    man.dir <- file.path(roxygen.dir, roxygen:::MAN.DIR)
    inst.dir <- file.path(roxygen.dir, roxygen:::INST.DIR)
    doc.dir <- file.path(inst.dir, roxygen:::DOC.DIR)
    namespace.file <- file.path(roxygen.dir, roxygen:::NAMESPACE.FILE)
    package.description <- file.path(package.dir, roxygen:::DESCRIPTION.FILE)
    roxygen.description <- file.path(roxygen.dir, roxygen:::DESCRIPTION.FILE)
    skeleton <- c(roxygen.dir, man.dir, doc.dir)
    if (copy.package) 
        roxygen:::copy.dir(package.dir, roxygen.dir, unlink.target = unlink.target, 
            overwrite = overwrite, verbose = FALSE)
    for (dir in skeleton) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    r.dir <- file.path(package.dir, roxygen:::R.DIR)
    files <- c(as.list(list.files(r.dir, pattern = "\\.(R|r)$", full.names = TRUE)),
               as.list(list.files(file.path(package.dir, "data"), pattern = "\\.(R|r)$", full.names = TRUE)))
    Rd <- {
        if (use.Rd2) 
            roxygen::make.Rd2.roclet(man.dir)
        else roxygen::make.Rd.roclet(man.dir)
    }
    do.call(Rd$parse, files)
    namespace <- roxygen::make.namespace.roclet(namespace.file)
    do.call(namespace$parse, files)
    collate <- roxygen::make.collate.roclet(merge.file = package.description, 
        target.file = roxygen.description)
    collate$parse.dir(r.dir)
}

buildRd <- function(rfile, rdfile) {
  Rd <- roxygen::make.Rd2.roclet(dirname(rdfile))
  Rd$parse(rfile)
}