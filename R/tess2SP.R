# From https://github.com/davesteps/randomFuns/blob/master/R/spatial.R


#' owin2Polygons
#' convert spatstat objects to sp classes
#'
#' @param x 
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
owin2Polygons <- function(x, id="1") {
  library(sp)
  stopifnot(is.owin(x))
  x <- as.polygonal(x)
  closering <- function(df) { df[c(seq(nrow(df)), 1), ] }
  pieces <- lapply(x$bdry,
                   function(p) {
                     Polygon(coords=closering(cbind(p$x,p$y)),
                             hole=is.hole.xypolygon(p))  })
  z <- Polygons(pieces, id)
  return(z)
}

#' tess2SP
#' convert spatstat objects to sp classes
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
tess2SP <- function(x) {
  library(sp)
  stopifnot(is.tess(x))
  y <- tiles(x)
  nam <- names(y)
  z <- list()
  for(i in seq(y))
    z[[i]] <- owin2Polygons(y[[i]], nam[i])
  return(SpatialPolygons(z))
}

#' owin2SP
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
owin2SP <- function(x) {
  library(sp)
  stopifnot(is.owin(x))
  y <- owin2Polygons(x)
  z <- SpatialPolygons(list(y))
  return(z)
}