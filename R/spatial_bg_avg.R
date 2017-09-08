#' spatial_bg_avg
#'
#' Find the spatial block group average, weighting each receptor by its spatial coverage of the block group.
#' @param values Receptor modeling results for averaging.
#' @param receptors MNRISKS receptor number.
#' @param bg_geoids Census block group geoid numbers to include in results. Default returns all block groups.
#' @keywords spatial average block group receptors mnrisks
#' @export
#' @examples
#' # For all block groups
#' bg_avg <- spatial_bg_avg(values    = df$concentration, 
#'                          receptors = df$receptors)
#'                          
# 
# 
spatial_bg_avg <- function(values       = NULL,
                           receptors    = NULL,
                           bg_geoids    = NULL,
                           results_only = FALSE) {
  
  if(is.null(values)) stop("Incorrect values passed to function. Set `value` to the column containing the modeling results")
  
  #-- Load receptor area fractions for each block group 
  rec_frx <- receptor_bg_areas() %>% ungroup()
  
  #-- Filter to selected block groups
  if(!is.null(bg_geoids)) {
    
    #-- Remove dash from block groups
    bg_geoids <- gsub("-", "", bg_geoids)
    
    rec_frx <- subset(rec_frx, geoid %in% bg_geoids)
  }
  
  
  #-- Create data frame for joining
  data <- dplyr::data_frame(mean_value = values,
                            receptor   = receptors)
  

  #-- Join area fractions to concentration data frame
  rec_frx <- dplyr::left_join(rec_frx, data, by = "receptor")
  

  #-- Set area weight of missing receptors to zero
  rec_frx$area_wt <- ifelse(is.na(rec_frx$mean_value), 0, rec_frx$area_wt)
  
 
  #-- Calculate weighted block group averages using area fractions
  bg_avg <- rec_frx %>% 
            dplyr::group_by(geoid) %>% 
            dplyr::summarise(sum_of_area_wts = sum(area_wt, na.rm = TRUE), 
                             mean_value      = sum(area_wt * mean_value, na.rm = TRUE) / sum_of_area_wts)
    
  #-- Set value of missing block groups to zero
  #bg_avg$value_avg  <- ifelse(is.na(bg_avg$value_avg), 0, bg_avg$value_avg)

  
  #-- If results_only is True, return mean_value column as vector
  if(results_only) return(bg_avg$mean_value)
  
  return(bg_avg)
  
}
