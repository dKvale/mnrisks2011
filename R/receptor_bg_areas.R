#' receptor_bg_areas
#'
#' MNRISKS 2011 receptor area coverage for each block gorup
#' @param bg_id Block group IDs to include. Default includes all.
#' @keywords receptors mnrisks
#' @export
#' @examples
#' # All receptors
#' rec_fractions <- receptor_bg_areas()
#' 
#' #For selected block groups
#' rec_fractions <- receptor_bg_areas(bg_id = c(271090017012))
# 

receptor_bg_areas <- function(bg_id = NULL) {
  
  # Load receptor fraction data frame
  recept_df <- get(data("receptor_bg_areas", envir = environment()))
  
  if(!is.null(bg_id)) recept_df <- subset(recept_df, geoid %in% bg_id)
  
  return(recept_df)
  
}
