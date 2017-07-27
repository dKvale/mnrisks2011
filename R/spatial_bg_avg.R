#' spatial_bg_avg
#'
#' Find the spatial block group average, weighting each receptor by its spatial coverage of the block group
#' @param data Data frame with lat / long coordinates and values for averaging.
#' @keywords spatial average block group receptors mnrisks
#' @export
#' @examples
#' # For all block groups
#' bg_avg <- spatial_bg_avg(data     = df, 
#'                          values   = "cancer_risk",
#'                          receptor = "receptor"
#'                          )
# 
# 
spatial_bg_avg <- function(data     = df,
                           values   = NULL,
                           receptor = "receptor"
                           ) {
  
  if(is.null(values)) stop("Incorrect values passed to function. Set `value` to the column containing the modeling results")
  
  #-- Load receptor area fractions for each block group 
  rec_frx <- receptor_bg_areas()
  
  #data <- dplyr::data_frame(value_avg   = data[ , values],
  #                          receptor    = data[ , receptor],
  #                          geoid      = gsub("-", "", data[ , geoid]))
  
  names(data)[grep(receptor, names(data))] <- "receptor"
  #names(data)[grep(geoid, names(data))]    <- "geoid"
  
  
  #-- Remove dash from block groups
  #data$geoid <- gsub("-", "", data$geoid)

  #-- Join area fractions to concentration data frame
  #rec_frx <- dplyr::left_join(rec_frx, data)
  
  rec_frx <- left_join(rec_frx, data[ , c(values, "receptor")])
  
  #-- Set value name for grouping
  names(rec_frx)[grep(values, names(rec_frx))] <- "value_avg"
  
  
  #-- Set area weight of missing receptors to zero
  rec_frx$area_wts   <- ifelse(is.na(rec_frx$value_avg), 0, rec_frx$area_wts)
  
 
  #-- Calculate weighted block group averages using area fractions
  bg_avg <- rec_frx %>% 
            group_by(geoid) %>% 
            summarise(sum_of_area_wts = sum(area_wts, na.rm = TRUE), 
                      value_avg       = sum(area_wts * value_avg, na.rm = TRUE) / sum_of_area_wts)
    
  #-- Set value of missing block groups to zero
  bg_avg$value_avg  <- ifelse(is.na(bg_avg$value_avg), 0, bg_avg$value_avg)
  
  #-- Set value name back to original w/ bg_avg
  names(bg_avg)[grep('value_avg', names(bg_avg))] <- paste0("bg_avg_", values)
  
                 
  return(bg_avg)
  
}
