#' Creates a mediawiki S3 object.
#'
#' Set the api url as parameter.
#'
#' @param api string pointing to the API url. Usually ends with api.php
#' 
#' @return an object of class "mediawiki"
#' @references 
#' \url{http://www.mediawiki.org/wiki/API}
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
mediawiki <- function(api="") {
  if (api=="") stop("No url to the api provided.")
  
  result <- list(api=api)
  class(result) <- "mediawiki"
  
  result
}

#' Prints information on a mediawiki API object.
#'
#' @param x     mediawiki S3 object
#' @param ...         unused (from generic)
#' @return used for its side effect.
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
print.mediawiki <-  function(x, ...) 
  cat(paste("mediawiki API object pointing to ", x$api, sep=""))

#' Queries a mediawiki database using its API.
#'
#' Use the mediawiki parameters titles, pageids, revids, prop, duplicatefiles, globalusage etc as parameters.
#' The output is considered as UTF-8.
#'
#' @param obj reference of the mediawiki object
#' @param ... the parameters to the API 
#'             titles         - A list of titles to work on
#'             pageids        - A list of page IDs to work on
#'             revids         - A list of revision IDs to work on
#'             prop           - Which properties to get for the titles/revisions/pageids
#'                              Values (separate with '|'): info, revisions, links, langlinks, images, imageinfo, templates, categories, extlinks,
#'                              categoryinfo, duplicatefiles, globalusage
#'             list           - Which lists to get
#'                              categorymembers, deletedrevs, embeddedin, imageusage, logevents, recentchanges, search, usercontribs, watchlist, 
#'                              watchlistraw, exturlusage, users, random, protectedtitles, globalblocks, abuselog, abusefilters
#'                              Values (separate with '|'): allimages, allpages, alllinks, allcategories, allusers, backlinks, blocks, 
#'             meta           - Which meta data to get about the site
#'                              Values (separate with '|'): siteinfo, userinfo, allmessages, globaluserinfo
#'             generator      - Use the output of a list as the input for other prop/list/meta items
#'                              NOTE: generator parameter names must be prefixed with a 'g', see examples.
#'                              One value: links, images, templates, categories, duplicatefiles, allimages, allpages, alllinks, allcategories, 
#'                              backlinks, categorymembers, embeddedin, imageusage, search, watchlist, watchlistraw, exturlusage, random, 
#'                              protectedtitles
#'             redirects      - Automatically resolve redirects
#'             indexpageids   - Include an additional pageids section listing all returned page IDs.
#'             export         - Export the current revisions of all given or generated pages
#'             exportnowrap 
#' 
#' @return a list representation of the output
#' @references 
#' \url{http://en.wikipedia.org/w/api.php}
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
#' @examples
#' wiki <- mediawiki("http://de.wikipedia.org/w/api.php")
#' query(wiki, prop="templates", titles="Soest")
#'
query.mediawiki <- function(obj, ...) {
  if (!require(RJSONIO)) stop("Could not load required library RJSONIO (install from Omegahat)")
  params <- list(...)
  url <- paste(obj$api, "?action=query&format=json", sep="")
  for (opt in names(params))
    url <- paste(url, URLencode(paste(opt, "=", params[[opt]], sep="")), sep="&")
  oldHTTPUserAgent <- getOption("HTTPUserAgent")
  options(HTTPUserAgent=paste("wzd / ", packageDescription("wzd")$Version, sep=""))
  res <- fromJSON(url)$query
  options(HTTPUserAgent=oldHTTPUserAgent)

  setEncoding <- function(x) {
    if (class(x)=="list") x <- sapply(x, setEncoding)
    if (class(x)=="character") Encoding(x) <- "UTF-8"
    x
  }  
  
  setEncoding(res)
}

# query(wiki, list="categorymembers", cmlimit="500", cmtitle="Kategorie:Ort_in_Deutschland")
# query(wiki, list="allusers", aulimit="500")









