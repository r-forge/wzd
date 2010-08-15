# ' todo
# ' - eine gute Signatur überlegen
# ' - nach bestehenden Lösungen suchen
# ' - implementieren
  # if (output=="xts") {
    # if (!require(xts)) stop("Could not load required library xts")
    # dataRaw <- read.csv(theUrl, encoding="UTF-8", na.strings=c("..", "-"), stringsAsFactor=FALSE)
    # dataTransformed <- t(as.matrix(dataRaw[,2:ncol(dataRaw)]))
    # if (!is.numeric(dataTransformed[1,])) {
      # dimSaved <- dim(dataTransformed)
      # dataTransformed <- as.numeric(gsub("[,']", '', dataTransformed))
      # dim(dataTransformed) <- dimSaved
    # }
    # colnames(dataTransformed) <- dataRaw[,1]
    # return(xts(dataTransformed, order.by=as.Date(paste(colnames(dataRaw)[2:ncol(dataRaw)],"-06-15", sep=""), format="X%Y-%m-%d")))
  # }
