

#' Get Study Area Transects
#'
#' Drop observations that are too old or off-transect based on specified criteria.
#' Converts location data, spatially joins with hexagon data, and filters by month.
#' Identifies hexes with sufficient survey effort in both study seasons.
#'
#' @param loc Locations_NPPSDv4.csv from the NPPSD
#' @param hex hex_x_ocean.shp hex polygon shapefile delineating study area boundaries
#' @param startYear The starting year for filtering location data.
#' @param effortThreshold The threshold for survey effort (as decimal percentage). Default value = 0.01.
#' @return A data frame containing transect data in appropriate timeframe and within the study area.
#' @import lubridate dplyr sf
#' @examples
#' loc <- read.csv("location_data.csv")
#' hex <- read_sf("hexagon_data.shp")
#' transects <- get_study_area_transects(loc, hex, startYear = 2008, effortThreshold = 0.02)
get_study_area_transects <- function(loc, hex, startYear = 2006, effortThreshold = 0.01) {
  # Drop observations that are too old or off-transect
  # Based on Kathy Kuletz's feedback, prior to 2007 is too old.
  newloc <- loc %>%
    mutate(
      date = lubridate::as_datetime(local_date_time),
      year = lubridate::year(date),
      month = lubridate::month(date)
    ) %>%
    filter(year >= startYear) %>%
    # Removing off-transect from location data
    filter(modified_survey_type != "Off Transect Observation") %>%
    droplevels()
  
  # Convert to SF and transform to Alaska Albers (epsg 3338), otherwise hexagons over -180:180 get warped
  locSF <- st_as_sf(newloc, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
    st_transform(crs = 3338)
  
  # Set up one df for bird obs and another df for survey effort
  locIn <- locSF %>%
    # Spatially join with hexagon data
    st_join(hex) %>%
    filter(!is.na(hex_id), month %in% c(6, 7, 8, 9, 10, 11))
  
  # Identify hexes with sufficient survey effort in both study seasons
  summLoc <- locIn %>%
    filter(month %in% c(6, 7, 8)) %>%
    group_by(hex_id, area_km) %>%
    summarize(survEff = sum(sample_area)) %>%
    filter(survEff > (as.numeric(area_km) * effortThreshold))
  
  fallLoc <- locIn %>%
    filter(month %in% c(9, 10, 11)) %>%
    group_by(hex_id, area_km) %>%
    summarize(survEff = sum(sample_area)) %>%
    filter(survEff > (as.numeric(area_km) * effortThreshold))
  
  sharedIDs <- intersect(unique(summLoc$hex_id), unique(fallLoc$hex_id))
  
  transectsIn <- st_drop_geometry(locIn[locIn$hex_id %in% sharedIDs,])
  
  if(!file.exists("./data_processed")){
    dir.create("./data_processed")
  }
  
  write.csv(transectsIn, paste0("./data_processed/transectsIn.csv"))
  return(transectsIn)
}

#' Calculate Multi-Taxa Hex Density
#'
#' Calculates hexagon density for multiple seabrid taxa groups based on observation data and transect information.
#' Generates a data frame with hexagon densities for each season and taxa.
#'
#' @param obs Observations_NPPSDv4.csv from the NPPSD.
#' @param transectsIn Output of get_study_area_transects - A data frame containing transect information.
#' @param taxaDf NPPSD_Bird_Codes_Only_Revised.csv - A data frame of 4-digit bird codes with indicators for taxonomic group inclusion.
#' @param startYear The starting year for filtering observation data.
#' @return A data frame containing hexagon density information for multiple taxa.
#' @import dplyr
#' @examples
#' obs <- read.csv("observation_data.csv")
#' transects <- read.csv("transect_data.csv")
#' taxaInfo <- read.csv("taxa_information.csv")
#' hex_density <- multi_taxa_hex_density(obs, transects, taxaInfo, startYear = 2010)
multi_taxa_hex_density <- function(obs, transectsIn, taxaDf, startYear = 2006) {
  listTaxaList <- list()
  
  for (i in 2:ncol(taxaDf)) {
    temp <- taxaDf$Code[which(taxaDf[, i] == 1)]
    listTaxaList[[i - 1]] <- temp
    names(listTaxaList)[i - 1] <- snake_to_label(colnames(taxaDf[i]))
    rm(temp)
  }
  
  finalDf <- get_hex_density(
    obs = obs,
    transectsIn = transectsIn,
    taxaList = listTaxaList[1]
  )
  
  for (i in 2:length(listTaxaList)) {
    temp <- get_hex_density(
      obs = obs,
      transectsIn = transectsIn,
      taxaList = listTaxaList[i]
    )
    
    finalDf <- left_join(finalDf, temp, by = c("hex_id", "season", "n_transects", "sample_area"))
    rm(temp)
  }
  
  return(finalDf)
}

#' Calculate Hexagon Density
#'
#' Calculates hexagon density based on observation data and transect information for a specific taxa.
#' Generates a data frame with hexagon densities for each season and taxa.
#'
#' @param obs Observations_NPPSDv4.csv from the NPPSD.
#' @param transectsIn Output of get_study_area_transects - A data frame containing transect information.
#' @param taxaList Derived within multi_taxa_hex_density
#' @param startYear The starting year for filtering observation data.
#' @return A data frame containing hexagon density information for the specified taxa.
#' @import dplyr
#' @examples
#' obs <- read.csv("observation_data.csv")
#' transects <- read.csv("transect_data.csv")
#' taxa_list <- c("taxa1", "taxa2")
#' hex_density <- get_hex_density(obs, transects, taxa_list, startYear = 2010)
get_hex_density <- function(obs, transectsIn, taxaList, startYear = 2006) {
  
  taxa <- unlist(taxaList)
  taxaLab <- label_to_snake(names(taxaList))
  
  obsIn <- obs %>%
    filter(on_off_tx == "ON") %>%
    filter(master_key %in% transectsIn$master_key) %>%
    filter(species_code %in% taxa) %>%
    group_by(master_key) %>%
    summarize(n_birds = sum(number))
  
  transectDensity <- left_join(transectsIn, obsIn, by = c("master_key")) %>%
    mutate(
      density = n_birds / sample_area,
      season = ifelse(
        month %in% c(6, 7, 8),
        "summer",
        ifelse(month %in% c(9, 10, 11), "fall", NA)
      )
    ) %>%
    select(master_key, hex_id, year, month, season, sample_area, n_birds, density)
  
  transectDensity$n_birds[is.na(transectDensity$n_birds)] <- 0
  transectDensity$density[is.na(transectDensity$density)] <- 0
  
  hexDensity1 <- transectDensity %>%
    mutate(n_birds = ifelse(n_birds > 10000, 10000, n_birds)) %>%
    group_by(hex_id, season) %>%
    summarize(
      density = mean(density),
      n_transects = n(),
      sample_area = sum(sample_area)
    ) %>%
    rename({{taxaLab}} := density)
  
  return(hexDensity1)
}

#' Prepare Traffic Data
#'
#' Reads, cleans, and processes traffic data files from a specified directory.
#' Reads shapefiles, cleans data, calculates traffic values, and combines results.
#'
#' @param fileDir The directory containing traffic data files.
#' @return A processed data frame containing traffic information.
#' @import dplyr purrr tidyr sf
#' @examples
#' traffic_data <- prep_traff("traffic_files/")
#' 
#' 
prep_traff <- function(fileDir) {
  fileList <- list.files(fileDir, pattern = ".shp")
  # Isolate month values of interest from file names
  hexes <- fileList[as.numeric(substr(fileList, 10, 11)) %in% c(6:11)]
  
  # Read in data
  temp <- lapply(hexes, function(x) {
    st_read(paste0(fileDir, x), quiet = TRUE) %>%
      st_drop_geometry()
  })
  
  hexAll <- do.call(rbind, temp)
  
  hexAll$month <- as.numeric(hexAll$month)
  hexAll$season <- ifelse(hexAll$month %in% 6:8, "summer", 
                   ifelse(hexAll$month %in% 9:11, "fall", NA))
  
  cols <- c("N_Hrs_Al", "Hrs_Al", "D_Hrs_Al")
  
  dfs <- lapply(cols, clean_traff_subsets, df = hexAll)
  t <- reduce(dfs, inner_join, by = c("hex_id", "season"))
  
  return(t)
}

#' Clean Traffic Subsets
#'
#' Cleans and processes a specific column in a data frame containing traffic data.
#' Replaces NA values with zeros and calculates total traffic values for hexagons.
#'
#' @param df The input data frame containing traffic data.
#' @param colName The name of the column to be processed.
#' @return A data frame with cleaned and processed traffic data.
#' @import dplyr
#' @examples
#' cleaned_data <- clean_traff_subsets(traffic_data, "N_Hrs_Al")
clean_traff_subsets <- function(df, colName) {
  temp <- df %>%
    dplyr::select(colName, hexID, season) %>%
    rename(hex_id = hexID)
  
  # Replace NA values with zero
  temp[is.na(temp)] <- 0
  
  # Calculate total vessel traffic across study period for months of interest
  newColName <- tolower(colName)
  
  # NEED TO ADD SEASONAL CALCS HERE...
  hexRes <- temp %>%
    group_by(hex_id, season) %>%
    summarise(new_col = sum(!!sym(colName))) %>%
    rename({{newColName}} := new_col)
}

#' Convert Snake Case to Label Case
#'
#' Converts a snake_case string to a title case (label case) string.
#'
#' @param x The input snake_case string.
#' @return A title case (label case) string.
#' @examples
#' converted_string <- snake_to_label("example_string")
snake_to_label <- function(x) {
  str_to_title(sub("_", " ", x))
}

#' Convert Label Case to Snake Case
#'
#' Converts a title case (label case) string to snake_case.
#'
#' @param x The input title case (label case) string.
#' @return A snake_case string.
#' @examples
#' converted_string <- label_to_snake("Example String")
label_to_snake <- function(x){
  tolower(sub(" ", "_", x))
  }
