library(targets, quietly = TRUE)
library(tarchetypes, quietly = TRUE)
library(tidyverse, quietly = TRUE)
library(sf, quietly = TRUE)
library(metR, quietly = TRUE)
library(ggpubr, quietly=TRUE)

# Read functions necessary for the workflow. 
scripts <- list.files("R/", full.names = TRUE)
sapply(scripts, source)

# Set target-specific options such as packages:
tar_option_set(packages = c("tidyverse", "sf", "metR", "ggpubr"), 
               error = "null")


# Load in files from desktop ----------------------------------------------
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


# Specify regional subets and taxa groups for analysis  ----------------------------------------------
regions <- data.frame(
  study_area = c("AllAlaska.shp", "BerChukBeau.shp", "KodiakPWS.shp", "Unimak.shp"),
  name = c("all-ak","nbs-cs", "goa", "aleut"), 
  title = c("All Alaska", "Northern Bering & Chukchi Seas", 
            "Gulf of Alaska", "Eastern Aleutians")
)

# List of species codes with binary indicator columns for belonging in a given taxa group
allSpp <- read.csv("./data_raw/NPPSD_Bird_Codes_Only_Revised.csv")


# Initial data cleaning and organization targets ----------------------------------------------

inits <- list(
  
  tar_target(studyTransects, 
             command = get_study_area_transects(loc=loc,
                                      hex=hex)), 
  tar_target(birdDat, 
             command = multi_taxa_hex_density(obs=obs, 
                                              transectsIn = studyTransects,
                                              taxaDf = allSpp)),
  tar_target(fullDf, 
             command = left_join(birdDat, traffDat)),
  tar_target(test,
             command = write.csv(fullDf,
                                 file = "./data_processed/full_df.csv",
                                 row.names = FALSE))
  )


# Mapped pipeline applies each target to each region ----------------------------------------------
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
                                      hex = studyHexes, 
                                      region_name = name)), 
  
  # Create bounding box for plotting
  tar_target(box, 
             command = st_as_sf(x = st_buffer(st_as_sfc(st_bbox(studyHexes)), 10000)),
             name = "boundary"),
  
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
                                            region_title = title)),
  
  # Create table of summary statistics for risk in a given region 
  tar_target(riskSummary, 
             command = risk_summary_table(nestDf = riskDfs, 
                                          region_title = title))) 

# Combine regional results  ----------------------------------------------

# Combine regional summary tables into master table
out_regions <- tar_combine(allRegions, 
                           mapped_pipeline[["regionSummary"]], 
                           command=bind_rows(!!!.x))

# Combine risk summary tables into master table
out_risk <- tar_combine(allRisk, 
                        mapped_pipeline[["riskSummary"]], 
                        command=bind_rows(!!!.x))

# Join targets into master list  ----------------------------------------------
list(inits, mapped_pipeline, out_regions, out_risk)



################################################################################
# Dumpster Zone (aka reminders and broken code)  ----------------------------------------------

# tar_validate() = make sure pipeline is ready to use
# tar_make() = run pipeline


# vis_network <- tar_visnetwork()
# interactive_widget <- htmlwidgets::saveWidget(vis_network, file = "./widgets/visnetwork_widget.html")
# png("visnetwork.png", width = 800, height = 600)
# print(vis_network)
# dev.off()
