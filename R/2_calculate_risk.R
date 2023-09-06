#' Calculate risk levels based on density and vessel traffic.
#'
#' This function calculates risk levels for all combinations of unique taxa groups and vessel activity metrics 
#' within a given region. 
#'
#' @param df Data frame containing bird and vessel traffic data .
#' @param shpfile Shapefile containing geometries and hex_ids of hexes with sufficient survey effort in a given region
#' @param region_name Name of the region being analyzed.
#' @return A data frame containing risk level calculations.
calculate_risk <- function(df, shpfile, region_name){
  
  # Excluded hexes outside of study area 
  dfnew <- df[df$hex_id %in% shpfile$hex_id,]
  
  birdCols <- colnames(dfnew)[!(colnames(dfnew) %in% c("hex_id", "season", "n_transects", "sample_area", "hrs_al", "d_hrs_al", "n_hrs_al"))]
  shipCols <- c("hrs_al", "d_hrs_al", "n_hrs_al")
  
  vars <- expand.grid(bird = birdCols, ship = shipCols)
  
  nestDf <- tibble()
  
  for(i in 1:length(vars$bird)){
    birdCol <- vars$bird[i]
    shipCol <- vars$ship[i]
    temp <- risk_subset(dfnew, birdCol, shipCol, region_name)
    nestDf <- rbind(nestDf, temp)
    rm(temp)
  }
  
  nestDf <- spread(nestDf, key="season", value="data")
  return(nestDf)
}

#' Calculate risk subset.
#'
#' This function calculates categorical and continuous measures of risk fora given taxa and  metric of vessel activity.
#'
#' @param df Data frame containing bird and vessel traffic data.
#' @param birdCol Column name containing density for a particular taxa group.
#' @param shipCol Column name containing vessel activity metric.
#' @param region_name Name of the region being analyzed.
#' @return A data frame containing calculated risk subset.
risk_subset <- function(df, birdCol, shipCol, region_name){
  
  oneBird <- df %>% 
    select(hex_id, season, all_of({{birdCol}}), all_of({{shipCol}})) %>% 
    rename(density = as.character({{birdCol}}),
           traff_hrs = as.character({{shipCol}}))
  
  oneBird$class_bird <- ifelse(oneBird$density == 0, 0, 
                        ifelse(oneBird$density<mean(oneBird$density),1,
                        ifelse(oneBird$density>=
                                 c(mean(oneBird$density)+
                                     sd(oneBird$density)),3,2)))
  
  oneBird$class_ship <- ifelse(oneBird$traff_hrs<mean(oneBird$traff_hrs),1,
                        ifelse(oneBird$traff_hrs>=c(mean(oneBird$traff_hrs)+
                                                          sd(oneBird$traff_hrs)),3,2))
  
  # Evaluate risk levels 
  oneBird$risk_cat <- ifelse(oneBird$class_bird == 1 & oneBird$class_ship == 1, "low",
                     ifelse(oneBird$class_bird == 1 & oneBird$class_ship == 2 | 
                            oneBird$class_bird == 1 & oneBird$class_ship == 3, "mediumSHIP",
                     ifelse(oneBird$class_bird == 2 & oneBird$class_ship == 1 | 
                            oneBird$class_bird == 3 & oneBird$class_ship == 1, "mediumBIRD",
                     ifelse(oneBird$class_bird == 3 & oneBird$class_ship == 2 | 
                            oneBird$class_bird == 2 & oneBird$class_ship == 3, "high",
                     ifelse(oneBird$class_bird == 2 & oneBird$class_ship == 2, "high",
                     ifelse(oneBird$class_bird == 3 & oneBird$class_ship == 3, "veryhigh", NA))))))
  
  oneBird$risk_cat <- factor(oneBird$risk_cat,
                          levels = c("low", "mediumBIRD", "mediumSHIP", "high", "veryhigh"))
  # Evaluate continuous risk 
  oneBird$quant_bird <- ecdf(oneBird$density)(oneBird$density)
  # BirdVal99 <- min(oneBird$density[which(oneBird$QuantBird > 0.99)])
  # oneBird$density_rescale1 <- ifelse(oneBird$QuantBird > 0.99, BirdVal99, oneBird$density)
  
  oneBird$quant_ship <- ecdf(oneBird$traff_hrs)(oneBird$traff_hrs)
  # ShipVal99 <- min(oneBird$traff_hrs[which(oneBird$quant_ship > 0.99)])
  # oneBird$traff_hrs_rescale1 <-  ifelse(oneBird$quant_ship > 0.99, ShipVal99, oneBird$traff_hrs)
  
  oneBird$bird_rescale <- log(oneBird$density+1)
  oneBird$ship_rescale <- log(oneBird$traff_hrs+1)
  
  # oneBird$density_rescale3 <- rescale(log(oneBird$density+1), to = c(0,10))
  # oneBird$traff_hrs_rescale3 <- rescale(log(oneBird$traff_hrs+1), to = c(0, 10))
  
  # oneBird$riskcont1 <- oneBird$density_rescale1*oneBird$traff_hrs_rescale1
  oneBird$risk_cont <- oneBird$bird_rescale*oneBird$ship_rescale
  
  filename <- paste0("./data_processed/risk-calcs_", 
                     sub("_", "-", {{birdCol}}), "_", 
                     sub("_", "-", {{shipCol}}), "_", 
                     sub("_", "-", region_name), ".csv")
  
  if(!file.exists("./data_processed")){
    dir.create("./data_processed")
  }
  
  write.csv(oneBird, filename , row.names=FALSE)
  
  oneBird$taxa <- as.character({{birdCol}})
  oneBird$traff <- as.character({{shipCol}})
  oneBird$region <- region_name
  
  t <- oneBird %>% dplyr::select(hex_id, season, taxa, region, traff, density, traff_hrs, risk_cat, risk_cont)
  tnew <- t %>% group_by(season, region, taxa, traff) %>% nest()
  return(tnew)
}

#' Prepare region hexagons.
#'
#' This function isolates hexes within a given region that have sufficient survey effort for inclusion in the study. 
#'
#' @param study_area File name of the study area bounding box shapefile.
#' @param hex hex_x_ocean.shp hex polygon shapefile delineating study area boundaries
#' @param fullDf Data frame containing vessel activity and seabird density measurements for a particular region 
#' @return A data frame of hexagons within the study area.
prep_region_hexes <- function(study_area, hex, fullDf){
  shpFile <- st_read(paste0("./data_raw/study_areas/", study_area), quiet=TRUE)
  inBounds <- hex[st_intersects(hex, shpFile, sparse=FALSE),]
  studyHex <- inBounds[inBounds$hex_id %in% unique(fullDf$hex_id),]
  
  return(studyHex)
}
