#' bg_data
#'
#' Load Census block group data for Minnesota.
#' @param geoid c(x,y,z) a vector of block group IDs to return
#' @keywords blockgroups block group census
#' @export
#' @examples
#' # All block groups
#' bgs <- bg_data()
#' 
#' # A single block group
#' bgs <- bg_data(geoid = c(271090017024))
# 
# 

bg_data <- function(geoid = NULL) {
  
  # Load block group data
  bgs <- get(data("bg_census", envir = environment()))
  
  # Filter block groups to given ID #'s  
  if(!is.null(geoid)) {
    
    bgs <- subset(bgs, GEOID %in% geoid)
    
  } 
  
  return(bgs)
  
}
