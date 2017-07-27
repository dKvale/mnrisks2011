#' make_voronoi_polys
#' 
#' Create Minnesota voronoi grid from receptors
#' @keywords spatial grid polygons voronoi
#' @export
#' @examples
#' # For all counties
#' make_voronoi_polys()
#' 
# 
# 
make_voronoi_polys <- function() {
  
  library(tigris)
  library(spatstat)
  library(spatstat.utils)
  library(sp)
  library(rgdal)
  library(rgeos)
  library(deldir)
  library(raster)
  library(dplyr)

  source("tess2SP.R")
  
  # Load county shapefiles
  counties <- counties(state = 'MN', cb = TRUE, resolution = '5m')
  #counties20 <- counties(state = 'MN', cb = TRUE, resolution = '20m')
  
  # Save projection info
  county_proj <- proj4string(counties)

  coordinates(counties)[1:5]
  
  
  # Load block groups
  bgs <- block_groups(state = 'MN', cb = F) # Detailed file
  #bgs2 <- block_groups(state = 'MN', cb = T)
  #bgs <- mnrisks2011::bg_shape()
  
  bg_ids <- bgs$GEOID
  
  bg_neighbors <- gTouches(bgs, byid = T)
  
  proj4string(bgs)  <- county_proj
  

  # Load receptors
  receptors <- mnrisks2011::receptors()
  #coordinates(receptors) <- ~Long + Lat
  #proj4string(receptors) <- county_proj

 
  # Create polygons for each block group 
  # Use poly-intersection from 'rgdal'
  bg <- bgs$GEOID[1]
  
  # For missing block group: 271090017024
  i <- grep(271090017024, bgs$GEOID)
  
  # Fewest receptors
  #i <- grep(270990006001, bgs$GEOID)
  
  #i <- grep(271450116001, bgs$GEOID)
  
  # Blank table for receptor fractions
  receptor_area_fractions <- data_frame()
  
  # Loop 1 block group at a time
  for(i in 1:length(bg_ids)) {
    
    bg <- bgs$GEOID[i]
    
    print(bg)
    
    # Create boundary for selected block group
    # that includes neighboring block groups
    bg_sub <- subset(bgs, GEOID %in% c(bg, bg_ids[bg_neighbors[i, ]]))
    
    # Create "owin" county file for spatstat 
    #bg_region <- as(counties, "owin")  
    #plot(bg_sub)
    
    bg_region <- as(bg_sub, "owin")  
    
    # Filter receptors to selected block group and neighbors
    bg_recepts <- subset(receptors,
                         geoid %in% c(bg, bg_ids[bg_neighbors[i, ]]))
    
    bg_recepts$row_id <- 1:nrow(bg_recepts)
   
    # Create "ppp" file for rgdal
    bg_ppp <- ppp(bg_recepts$long, bg_recepts$lat, window = bg_region)
    
    plot(bg_ppp)
    
    # Create voronoi polygons
    v_polys <- deldir(bg_recepts$long, bg_recepts$lat)
    
    plot(bg_sub)
    
    plot(v_polys, add = T)
    
    v_polys <- dirichlet(bg_ppp)
    
    plot(bg_sub)
    plot(subset(bgs, GEOID == bg), add = T, col ="blue")
    plot(v_polys, add = T)
    
    #text(bg_recepts$lat ~ bg_recepts$long, labels = bg_recepts$row_id, col = "red")
    
    text(bg_recepts$lat ~ bg_recepts$long, labels = bg_recepts$receptor, col = "blue", cex = 0.6)
    
    points(bg_recepts$lat ~ bg_recepts$long, col = "black")
         
    v_poly <- tess2SP(v_polys)
    
    plot(v_poly)
    
    names(v_poly)
    
    # Remove overlaps
    v_poly <- gBuffer(v_poly, byid=TRUE, width = 0.0000001)
    
    proj4string(v_poly) <- county_proj
    
    # Restrict to selected blockgroup only
    bg_sub <- subset(bgs, GEOID %in% bg)
    
    plot(bg_sub)
    
    v_poly <- gIntersection(v_poly, bg_sub, byid = T)
    
    print(plot(v_poly))
    
    
    # Assign receptor numbers and polygon areas
    v_poly_ids <- sapply(names(v_poly), 
                         function(x) as.numeric(strsplit(x, " ")[[1]][1]),
                         USE.NAMES = F)
    
    #crs(v_poly)
    area(v_poly)
    
    v_poly_df <- data.frame(geoid   = bg,
                            receptor = bg_recepts[v_poly_ids, ]$receptor,
                            area     = area(v_poly))
    
    v_poly_df$area_frx <- v_poly_df$area / sum(v_poly_df$area, na.rm = T) 
    
    sub_recepts <- bg_recepts[v_poly_df$geoid, ]
    
    print(text(getSpPPolygonsLabptSlots(v_poly), labels = v_poly_df$receptor, cex = 0.8))
    
    # Assign row names for spatial join
    if(FALSE) {
      row.names(v_poly_df) <- row.names(v_poly)
      
      v_poly <- SpatialPolygonsDataFrame(v_poly, v_poly_df)
      
      plot(v_poly)
      
      v_poly@data
      
      bg_voronois  <- spRbind(cnty_grid, state_grid)
    }
    
    # Collapse data frame to one row per block group
    v_summary <- summarize(v_poly_df,
                           geoid     = geoid [1],
                           receptors = list(receptor),
                           area_wts  = list(area_frx))
    
    receptor_area_fractions <- bind_rows(v_summary, receptor_area_fractions)
    
    Sys.sleep(0.3)
      
  }
  
  # Round
  receptor_area_fractions <- receptor_area_fractions %>% 
                             rowwise() %>% 
                             mutate(area_wts = list(round(area_wts, 5)))
  
  # Save
  save(receptor_area_fractions, file = "data/receptor_area_fractions.rdata")
  
}

##
