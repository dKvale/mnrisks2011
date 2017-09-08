#' read_rsk
#'
#' MNRISKS 2011 modeled receptors 
#' @param file Path to .rsk file
#' @keywords rsk results mnrisks
#' @export
#' @examples \dontrun {
#' # Read in a single .rsk file
#' acrolein <- read_rsk("acrolein_resuls.rsk")
#' }
# 

read_rsk <- function(file, scenarios = NULL) {
  
  # Input error tests
  if(is.null(file)) error("Please provide a file path.")
  
  # Connect to file path
  # If file path includes ".zip", connect first file in zip archive
  if(grepl("[.zip]", file)) {
    
    con <- unz(file, unzip(file, list = T)[[1]])
    
  } else {
    
    con <- file(file)
    
  } 
  
  on.exit(close(con)) 
  
  
  # Read rsk file as text
  df <- readLines(con)
     
  # Drop empty lines and blockgroup headers
  df <- df[nchar(df) > 12]
  
  # Add line breaks
  df <- paste(df, "\n")
  
  # Convert to data frame
  df <- readr::read_csv(paste(df, collapse = ""), col_names = FALSE)
  
  # Drop extra columns
  df <- df[ , -c(2,3,6,9,10,13)]
  
  # Set header names
  names(df)[1:7]  <- c("receptor", "long", "lat", "cas", "pollutant", "facility", "scc")
  
  names(df)[8:ncol(df)] <- paste("scenario", 1:(ncol(df) - 7), sep = "_")
  
  if(!is.null(scenarios)) {
    
    if(length(scenarios) < ncol(df) - 7) {
      scenarios <- paste(scenarios, 1:(ncol(df) - 7), sep = "_")
    }
    
    names(df)[8:ncol(df)] <- scenarios[1:(ncol(df) - 7)]
    
  }
    
  # Reduce digits
  df$lat  <- round(as.numeric(df$lat), 4)
  
  df$long <- round(as.numeric(df$long), 4)
  
  # Drop rows with missing receptor number
  df$receptor <- as.numeric(df$receptor)
  
  df <- subset(df, !is.na(receptor))
  
  # Add block group IDs
  receptors <- get_receptors()
  
  df <- dplyr::left_join(df, receptors[ , c(1:2,7)], by = "receptor")
  
  # Organize columns
  df <- df[ , c(1, (ncol(df)-1), ncol(df), 2:(ncol(df)-2))]
  
  # Clean white space
  df$cas  <- gsub("^\\s+|\\s+$", "", df$cas) 
  
  return(df)
  
}
