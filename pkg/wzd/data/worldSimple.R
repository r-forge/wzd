#' Spatial dataset for world maps.
#'
#' The data is taken from Bjorn Sandvik's site www.thematicmapping.org and processed using Roger Bivand's instructions on 
#' spatial.nhh.no/R/etc/world_map_intro.html.
#' 
#' Bjorn Sandvik's data are protected by a CC-SA license.
#'
#' @name worldSimple-data
#' @aliases worldSimple
#' @docType data
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
#' @examples
#' data(worldSimple)
#' spplot(worldSimple, "POP2005")
worldSimple <- local({
  # extract to tempdir?
  if (!require(sp)) stop("Could not load required library sp")
  if (!require(rgdal)) stop("Could not load required library rgdal")
  if (!file.exists("./worldSimple")) {
    res <- unzip("worldSimple.zip", files = NULL, list = FALSE, overwrite = TRUE, junkpaths = FALSE, exdir = "worldSimple")
    if (length(res)==0) stop("Could not extract map data from zip file. No write access?")
  }

  res <- readOGR("worldSimple", "TM_WORLD_BORDERS_SIMPL-0.3")
  spChFIDs(res, as.character(res$ISO3))
})