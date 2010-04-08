\name{query.mediawiki}
\alias{query.mediawiki}
\title{Queries a mediawiki database using its API.}
\usage{query.mediawiki(w, ...)
}
\description{Queries a mediawiki database using its API.}
\details{Use the mediawiki parameters titles, pageids, revids, prop, duplicatefiles, globalusage etc as parameters.
The output is considered as UTF-8.}
\value{a list representation of the output}
\references{\url{http://en.wikipedia.org/w/api.php}}
\author{Karsten Weinert \email{k.weinert@gmx.net}}
\arguments{\item{...}{the parameters to the API 
titles - A list of titles to work on
pageids        - A list of page IDs to work on
revids         - A list of revision IDs to work on
prop           - Which properties to get for the titles/revisions/pageids
Values (separate with '|'): info, revisions, links, langlinks, images, imageinfo, templates, categories, extlinks,
categoryinfo, duplicatefiles, globalusage
list           - Which lists to get
categorymembers, deletedrevs, embeddedin, imageusage, logevents, recentchanges, search, usercontribs, watchlist, 
watchlistraw, exturlusage, users, random, protectedtitles, globalblocks, abuselog, abusefilters
Values (separate with '|'): allimages, allpages, alllinks, allcategories, allusers, backlinks, blocks, 
meta           - Which meta data to get about the site
Values (separate with '|'): siteinfo, userinfo, allmessages, globaluserinfo
generator      - Use the output of a list as the input for other prop/list/meta items
NOTE: generator parameter names must be prefixed with a 'g', see examples.
One value: links, images, templates, categories, duplicatefiles, allimages, allpages, alllinks, allcategories, 
backlinks, categorymembers, embeddedin, imageusage, search, watchlist, watchlistraw, exturlusage, random, 
protectedtitles
redirects      - Automatically resolve redirects
indexpageids   - Include an additional pageids section listing all returned page IDs.
export         - Export the current revisions of all given or generated pages
exportnowrap}
}
\examples{wiki <- mediawiki("http://de.wikipedia.org/w/api.php")
query(wiki, prop="templates", titles="Soest")
query(wiki, list="categorymembers", cmlimit="500", cmtitle="Kategorie:Ort_in_Deutschland")
query(wiki, list="allusers", aulimit="500")}

