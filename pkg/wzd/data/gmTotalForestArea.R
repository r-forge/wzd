gmTotalForestArea <- local({
  if (!require(xts)) stop('Could not load required library xts')
  dataCsv <- 'gmTotalForestArea.csv'
  if (!file.exists(dataCsv))
    if (download.file('http://spreadsheets.google.com/pub?key=pp59adS3CHWeB1N1HlpFQVQ&gid=0&output=csv', dataCsv, method='internal')!=0) stop('Could not download dataset.')
  dataRaw <- read.csv(dataCsv, encoding='UTF-8', na.strings=c('..', '-'), stringsAsFactor=FALSE)
  delete <- is.null(getOption('wzd.cache.gapminder')) || getOption('wzd.cache.gapminder')==FALSE
  if (delete) file.remove(dataCsv)
  dataTransformed <- t(as.matrix(dataRaw[,2:ncol(dataRaw)]))
  colnames(dataTransformed) <- dataRaw[,1]
  xts(dataTransformed, order.by=as.Date(paste(colnames(dataRaw)[2:ncol(dataRaw)],'-06-15', sep=''), format='X%Y-%m-%d'))
})