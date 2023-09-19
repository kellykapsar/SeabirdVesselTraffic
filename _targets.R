library(targets, quietly = TRUE)
library(tarchetypes, quietly = TRUE)
library(tidyverse, quietly = TRUE)
library(sf, quietly = TRUE)
library(metR, quietly = TRUE)
library(ggpubr, quietly=TRUE)
library(scales, quietly = TRUE)

# Read functions necessary for the workflow. 
scripts <- list.files("R/", full.names = TRUE)
sapply(scripts, source)

# Set target-specific options such as packages:
tar_option_set(packages = c("tidyverse", "sf", "metR", "ggpubr", "scales"), 
               error = "null")


# Load in files from desktop ---------------------------------------------------
# Hexagon polys
hex <- st_read("./data_raw/hex_x_ocean/hex_x_ocean.shp", quiet = TRUE) %>% 
  select(hexID) %>%
  rename(hex_id = hexID) %>% 
  mutate(area_km = c(st_area(.)/1000000)) 

# Basemap for plotting
basemap <- st_read("./data_raw/basemap/AK_CAN_RUS.shp", quiet=TRUE) 

# Seabird data from NPPSD v4.0
loc <- read.csv("./data_raw/NPPSD_v4.0/Locations_NPPSDv4.csv") 
obs <- read.csv("./data_raw/NPPSD_v4.0/Observations_NPPSDv4.csv")

# Vessel traffic data
traffDat <- read.csv("./data_processed/traffic_data/traffic_clean.csv")


# Specify regional subets and taxa groups for analysis  ------------------------
regions <- data.frame(
  study_area = c("AllAlaska.shp", "BerChukBeau.shp", "KodiakPWS.shp", "Unimak.shp"),
  name = c("all-ak","nbs-cs", "goa", "aleut"), 
  title = c("All Alaska", "Northern Bering & Chukchi Seas", 
            "Gulf of Alaska", "Eastern Aleutians")
)

# List of species codes with binary indicator columns for belonging in a given taxa group
allSpp <- read.csv("./data_raw/NPPSD_Bird_Codes_Only_Revised.csv")


# Initial data cleaning and organization targets -------------------------------

inits <- list(
  
  # Aggregate traffic data 
  tar_target(traffDat, 
             command = prep_traff(fileDir = "D:/AIS_V2_DayNight_60km6hrgap/Hex_DayNight_Hours/")),
  
  # Clean transects
  tar_target(studyTransects, 
             command = get_study_area_transects(loc=loc,
                                                hex=hex)), 
  
  # Remove off transect, out of study area, and outside of study period observations
  tar_target(birdDat, 
             command = multi_taxa_hex_density(obs=obs, 
                                              transectsIn = studyTransects,
                                              taxaDf = allSpp)),
  
  # Generate complete dataframe of vessel activity and seabird density in each hex 
  tar_target(fullDf, 
             command = left_join(birdDat, traffDat)),
  
  # Calculate percentage of total observations for each focal taxa group and for all combined 
  tar_target(propObs, 
             command = focal_taxa_pct_obs(fullDf)),
  
  # Save data frame
  tar_target(test,
             command = write.csv(fullDf,
                                 file = "./data_processed/full_df.csv",
                                 row.names = FALSE))
  )


# Mapped pipeline applies each target to each region ---------------------------
mapped_pipeline <- tar_map(
  values = regions,
  names = "name",
  unlist = FALSE,
  
  # Isolate hexes from region with sufficient survey effort
  tar_target(studyHexes, 
             command = prep_region_hexes(study_area, hex, fullDf)),
  
  # Calculate categorical and continuous risk values 
  tar_target(riskDfs, 
             command = calculate_risk(df = fullDf, 
                                      shpfile = studyHexes, 
                                      region_name = name)), 
  
  # Create bounding box for plotting
  tar_target(box, 
             command = st_as_sf(st_buffer(st_as_sfc(st_bbox(studyHexes)), 10000),
             name = name)),
  
  # Crop basemape to region for plotting
  tar_target(basemapnew, 
             command = st_crop(x = basemap, 
                               y = box)),
  
  # Create and save categorical risk maps 
  tar_target(catPlots, 
             command = plot_risk_cat(nestDf = riskDfs, 
                                     region_name = name, 
                                     hex = studyHexes, 
                                     basemap = basemapnew, 
                                     box = box)), 
  
  # Create and save continuous risk maps 
  tar_target(conPlots, 
             command = plot_risk_con(nestDf = riskDfs, 
                                     region_name = name, 
                                     hex = studyHexes, 
                                     basemap = basemapnew, 
                                     box = box)), 
  
  # Create and save seabird density  maps 
  tar_target(birdPlots, 
             command = plot_bird(nestDf = riskDfs, 
                                  region_name = name, 
                                  hex = studyHexes, 
                                  basemap = basemapnew, 
                                  box = box)), 
  
  # Create and save vessel traffic  maps 
  tar_target(traffPlots, 
             command = plot_traff(nestDf = riskDfs, 
                                  region_name = name, 
                                  hex = studyHexes, 
                                  basemap = basemapnew, 
                                  box = box)), 
  
  # Create table of summary statistics for a region 
  tar_target(regionSummary, 
             command = region_summary_table(fullDf = fullDf,
                                            nestDf = riskDfs,
                                            hex = studyHexes, 
                                            region_title = title)),
  
  # Create table of summary statistics for risk in a given region 
  tar_target(riskSummary, 
             command = risk_summary_table(nestDf = riskDfs, 
                                          region_title = title)), 
  
  tar_target(taxaBarGraph, 
             command = bar_graph_taxa(df = riskSummary,
                                      region_name = name)), 
  tar_target(jointRiskMap, 
             command = plot_joint_high_risk(nestDf = riskDfs, 
                                            region_name = name, 
                                            hex = studyHexes, 
                                            basemap = basemapnew, 
                                            box = box))) 

# Combine regional results  ----------------------------------------------------

# Combine regional summary tables into master table
out_regions <- tar_combine(allRegions, 
                           mapped_pipeline[["regionSummary"]], 
                           command=bind_rows(!!!.x))

# Combine risk summary tables into master table
out_risk <- list(tar_combine(allRisk, 
                        mapped_pipeline[["riskSummary"]], 
                        command=bind_rows(!!!.x)), 
            tar_combine(fullRisk, 
                        mapped_pipeline[["riskDfs"]], 
                        command=bind_rows(!!!.x)))

# Create master sf object with all study area bounding boxes
out_study <- tar_combine(allBoxes, 
                        mapped_pipeline[["box"]], 
                        command=bind_rows(!!!.x))

combined_summaries <- list(tar_target(regionBarGraph,
                                      command = bar_graph_regions(df = allRisk)), 
                           
                           tar_target(nightBarGraph, 
                                      command = risk_bar_graph_night(df = allRisk)),
                           tar_target(allRiskTable, 
                                      command = make_joint_risk_table(df = allRisk)), 
                           tar_target(allRegionTable, 
                                      command = make_joint_region_table(df = allRegions))
)

# Join targets into master list  ----------------------------------------------
list(inits, mapped_pipeline, out_regions, out_risk, out_study, combined_summaries)



################################################################################
# Dumpster Zone (aka reminders and broken code)  -------------------------------

# tar_validate() = make sure pipeline is ready to use
# tar_make() = run pipeline


# vis_network <- tar_visnetwork()
# interactive_widget <- htmlwidgets::saveWidget(vis_network, file = "./widgets/visnetwork_widget.html")
# png("visnetwork.png", width = 800, height = 600)
# print(vis_network)
# dev.off()
