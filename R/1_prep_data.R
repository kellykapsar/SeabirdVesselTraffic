
get_study_area_transects <- function(loc, hex, startYear = 2006, effortThreshold = 0.01){
  
  # Drop observations that are too old or off-transect
  # Based on Kathy Kuletz's feedback, prior to 2007 is too old.
  newloc <- loc %>%
    mutate(date = lubridate::as_datetime(local_date_time), 
           year = lubridate::year(date), 
           month = lubridate::month(date)) %>% 
    filter(year >= startYear) %>%
    # Removing off-transect from location data
    filter(modified_survey_type != "Off Transect Observation") %>%
    droplevels()
  
  # convert to SF and transform to Alaska Albers (epsg 3338), otherwise hexagons over -180:180 get warped
  locSF <- st_as_sf(newloc,coords = c("longitude","latitude"),crs=4326, remove = FALSE) %>%
    st_transform(crs=3338)
  
  # Set up one df for bird obs and another df for survey effort
  locIn <- locSF %>%
    # Spatially join with hexagon data 
    st_join(hex) %>%
    filter(!is.na(hex_id), 
           month %in% c(6,7,8,9,10,11))
  
  # Identify hexes with sufficient survey effort in both study seasons   
  summLoc <- locIn %>% 
    filter(month %in% c(6,7,8)) %>% 
    group_by(hex_id, area_km) %>% 
    summarize(survEff = sum(sample_area)) %>% 
    filter(survEff > (as.numeric(area_km)*effortThreshold))
  fallLoc <- locIn %>% 
    filter(month %in% c(9,10,11)) %>% 
    group_by(hex_id, area_km) %>% 
    summarize(survEff = sum(sample_area)) %>% 
    filter(survEff > (as.numeric(area_km)*effortThreshold))
  
  sharedIDs <- intersect(unique(summLoc$hex_id), unique(fallLoc$hex_id))
  
  transectsIn <- st_drop_geometry(locIn[locIn$hex_id %in% sharedIDs,])

  write.csv(transectsIn, paste0("./data_processed/transectsIn.csv"))
  return(transectsIn)
}

multi_taxa_hex_density <- function(obs, transectsIn, taxaDf, startYear = 2006){
  
 listTaxaList <- list()
 
 for(i in 2:ncol(taxaDf)){
  temp <- taxaDf$Code[which(taxaDf[,i] == 1)]
  listTaxaList[[i-1]] <- temp
  names(listTaxaList)[i-1] <- snake_to_label(colnames(taxaDf[i]))
  rm(temp)
  }
  
 finalDf <- get_hex_density(obs=obs, 
                            transectsIn = transectsIn, 
                            taxaList = listTaxaList[1])
 for(i in 2:length(listTaxaList)){
   temp <- get_hex_density(obs=obs, 
                           transectsIn = transectsIn, 
                           taxaList = listTaxaList[i])
  finalDf <- left_join(finalDf, temp, by = c("hex_id", "season", "n_transects", "sample_area"))
  rm(temp)
  }
 return(finalDf)
}

get_hex_density <- function(obs, transectsIn, taxaList, startYear = 2006){
  
  taxa <- unlist(taxaList)
  taxaLab <- label_to_snake(names(taxaList))
  
  obsIn <- obs %>% 
    filter(on_off_tx == "ON") %>% 
    filter(master_key %in% transectsIn$master_key) %>% 
    filter(species_code %in% taxa) %>% 
    group_by(master_key) %>% 
    summarize(n_birds = sum(number))
  
  transectDensity <- left_join(transectsIn, obsIn, by=c("master_key")) %>% 
    mutate(density = n_birds/sample_area, 
           season = ifelse(month %in% c(6,7,8), "summer", 
                    ifelse(month %in% c(9,10,11), "fall", NA))) %>% 
    select(master_key, hex_id, year, month, season, sample_area, n_birds, density)
  transectDensity$n_birds[is.na(transectDensity$n_birds)] <- 0
  transectDensity$density[is.na(transectDensity$density)] <- 0
  
  hexDensity1 <- transectDensity %>% 
    mutate(n_birds = ifelse(n_birds > 10000, 10000, n_birds)) %>%
    group_by(hex_id, season) %>% 
    summarize(density = mean(density),
              n_transects = n(), 
              sample_area = sum(sample_area)) %>% 
    rename({{taxaLab}} := density)
  # hexDensity2 <- transectDensity %>% group_by(hex_id, season) %>% summarize(density = sum(n_birds)/sum(sample_area))
  # hexDensity3 <- transectDensity %>% mutate(n_birds = ifelse(n_birds > 10000, 10000, n_birds)) %>%  group_by(hex_id, season) %>% summarize(density = sum(n_birds)/sum(sample_area))
  return(hexDensity1)
}

prep_traff <- function(filedir){
  
  fileList <-list.files(fileDir, pattern=".shp")
  # Isolate month values of interest from file names 
  hexes <- fileList[as.numeric(substr(fileList,10,11)) %in% c(6:11)]
  
  # Read in data 
  temp <- lapply(hexes, function(x){st_read(paste0(hexdir,x), quiet=TRUE) %>% st_drop_geometry()})
  hexAll <- do.call(rbind, temp)

  cols <- c("N_Hrs_Al", "Hrs_Al", "D_Hrs_Al")
  dfs <- lapply(cols, clean_traff_subsets, df=hexAll)
  
  t <- reduce(dfs, inner_join, by="hex_id")
  
  return(t)
}

clean_traff_subsets <- function(df, colName){
  temp <- df %>% dplyr::select(colName, hexID) %>% rename(hex_id = hexID)
  
  # Replace NA values with zero 
  temp[is.na(temp)] <- 0
  
  # Calculate total vessel traffic across study period for months of interest 
  newColName <- tolower(colName)
  
  # NEED TO ADD SEASONAL CALCS HERE... 
  hexRes <- temp %>%
    group_by(hex_id) %>%
    summarise(new_col = sum(!!sym(colName))) %>% 
    rename({{newColName}} := new_col)
}

snake_to_label <- function(x){str_to_title(sub("_", " ",x))}
label_to_snake <- function(x){tolower(sub(" ", "_", x))}

