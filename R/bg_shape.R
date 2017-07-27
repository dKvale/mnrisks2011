#' bg_shape
#'
#' Load Census block group shapefile for Minnesota.
#' @param geoid c(x,y,z) a vector of block group IDs to return
#' @keywords blockgroups block group census
#' @export
#' @examples
#' # All block groups
#' bgs <- bg_shape()
#' 
#' # A single block group
#' bgs <- bg_shape(geoid = c(271090017024))
# 
# 

bg_shape <- function(geoid = NULL) {
  
  # Load shapefile
  bgs <- get(data("bg_shapes", envir = environment()))
  
  # Filter block groups to given ID #'s  
  if(!is.null(geoid)) {
    
    bgs <- subset(bgs, GEOID %in% geoid)
    
  } 
  
  return(bgs)
  
}
