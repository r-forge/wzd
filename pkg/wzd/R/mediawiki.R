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
  if (api=="") error("No url to the api provided.")
  
  result <- list(api=api)
  class(result) <- "mediawiki"
  
  result
}

#' Queries a mediawiki database using its API.
#'
#' Use the mediawiki parameters titles, pageids, revids, prop, duplicatefiles, globalusage etc as parameters.
#' The output is considered as UTF-8.
#'
#' @param ... the parameters to the API 
#'             titles - A list of titles to work on
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
#' query(wiki, list="categorymembers", cmlimit="500", cmtitle="Kategorie:Ort_in_Deutschland")
#' query(wiki, list="allusers", aulimit="500")
query.mediawiki <- function(w, ...) {
  if (!require(RJSONIO)) error("Could not load required library RJSONIO (install from Omegahat)")
  params <- list(...)
  url <- paste(w$api, "?action=query&format=json", sep="")
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


# http://rosettacode.org/mw/api.php?action=query&format=jsonfm&cmtitle=Category:Solutions_by_Programming_Language&list=categorymembers&cmlimit=500

# Very often, you will not get all the data you want in one request. To continue the request, you can use the provided query-continue value. Using the query-continue value
# api.php ? action=query & list=allcategories & acprefix=List%20of
# <?xml version="1.0" encoding="utf-8"?>
# <api>
  # <query-continue>
    # <allcategories acfrom="List of Baptist sub-denominations" />
  # </query-continue>
  # <query>
    # <allcategories>
      # <c>List of &quot;M&quot; series military vehicles</c>
      # <c>List of Alternative Rock Groups</c>
      # <c>List of Alumni of Philippine Science High School</c>
      # <c>List of American artists</c>
      # <c>List of Anglicans and Episcopalians</c>
      # <c>List of Arizona Reptiles</c>
      # <c>List of Artists by record label</c>
      # <c>List of Australian Anglicans</c>
      # <c>List of Bahá'ís</c>
      # <c>List of Balliol College people</c>
    # </allcategories>
  # </query>
# </api>
# You can now use acfrom=List%20of%20Baptist%20sub-denominations to get the next ten categories.
# When using a generator, you might get two query-continue values, one for the generator and one for the 'regular' module. In this case you need to continue the 'regular' module first (with the old value of generator-continue) until it runs out, then continue the generator. You should not set both query-continue parameters together.


# http://rosettacode.org/mw/api.php?action=query&format=json&list=categorymembers&cmlimit=500&cmtitle=Category:Visual_Basic_User
# http://rosettacode.org/mw/api.php?action=query&list=allusers&aulimit=500
# http://rosettacode.org/mw/api.php?action=query&prop=templates&titles=User:Short_Circuit
# http://rosettacode.org/mw/api.php?action=query&prop=revisions&titles=User:Short_Circuit&rvprop=content
# http://rosettacode.org/mw/api.php?action=query&generator=templates&titles=User:Short_Circuit&prop=info

# http://rosettacode.org/mw/api.php?action=query&prop=revisions&titles=User:Short_Circuit&rvprop=content
# http://rosettacode.org/wiki/


# http://de.wikipedia.org/w/api.php?action=query&list=categorymembers&cmlimit=500&cmtitle=Kategorie:Ort%20im%20Kreis%20Soest
# http://de.wikipedia.org/w/api.php?action=query&list=categorymembers&cmlimit=500&cmtitle=Kategorie:Ort%20in%20Nordrhein-Westfalen
# http://de.wikipedia.org/w/api.php?action=query&list=categorymembers&cmlimit=500&cmtitle=Kategorie:Ort_in_Deutschland

# http://de.wikipedia.org/w/api.php?action=query&prop=templates&titles=Soest










