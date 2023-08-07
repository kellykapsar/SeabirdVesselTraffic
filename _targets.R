library(targets, quietly = TRUE)
library(tarchetypes, quietly = TRUE)
library(tidyverse, quietly = TRUE)
library(sf, quietly = TRUE)
library(metR, quietly = TRUE)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(data_summary) to view the results.

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
scripts <- list.files("R/", full.names = TRUE)
sapply(scripts, source)



# Set target-specific options such as packages:
tar_option_set(packages = c("tidyverse", "sf", "metR"), 
               error = "null")

##########################################
# End this file with a list of target objects.

# params <- tribble(
#   ~study_area, ~name,
#   "BerChukBeau.shp",  "nbs_cs",    
#   "KodiakPWS.shp",  "goa"
# ) 
params <- data.frame(
  study_area = c("AllAlaska.shp", "BerChukBeau.shp", "KodiakPWS.shp", "Unimak.shp"),
  name = c("all-ak","nbs-cs", "goa", "aleut"), 
  title = c("All Alaska", "Northern Bering & Chukchi Seas", 
            "Gulf of Alaska", "Eastern Aleutians")
)

####################################
hex <- st_read("./data_raw/hex_x_ocean/hex_x_ocean.shp", quiet = TRUE) %>%
  select(hexID) %>%
  rename(hex_id = hexID) %>% 
  mutate(area_km = c(st_area(.)/1000000)) 

# Load in basemap for plots
basemap <- st_read("./data_raw/basemap/AK_CAN_RUS.shp", quiet=TRUE) 

# Load in seabird data from NPPSD v3.0
loc <- read.csv("./data_raw/NPPSD_v4.0/Locations_NPPSDv4.csv") 
obs <- read.csv("./data_raw/NPPSD_v4.0/Observations_NPPSDv4.csv")
# studyarea <- st_read("./data_raw/study_areas/KodiakPWS.shp", quiet = TRUE)
traffDat <- read.csv("./data_processed/traffic_data/traffic_clean.csv")

allSpp <- read.csv("./data_raw/NPPSD_Bird_Codes_Only_Revised.csv")


#########################################
# inits <- list(tar_target(studyTransects, 
#                     get_study_area_transects(loc=loc,
#                                              hex=hex)), 
#          tar_target(birdDat, 
#                     multi_taxa_hex_density(obs=obs, 
#                                            transectsIn = studyTransects,
#                                            taxaDf = allSpp)))
# joins <- tar_target(fullDf, 
#                     left_join(birdDat, traffDat))
# writeDf <- tar_target(test, 
#                     write.csv(fullDf, 
#                               file = "./data_processed/full_df.csv", 
#                               row.names = FALSE))
# # NEED TO GET CALCULATE_RISK FUNCTION WORKING IN A MAPPED TARGET 
# # ...ONE NESTED DF FOR EACH STUDY AREA... 
# testtar <- tar_target(riskDf, 
#                       calculate_risk(df = fullDf, shpfile = hex, region_name = "all-ak"))
# 

inits <- list(tar_target(studyTransects, 
               get_study_area_transects(loc=loc,
                                        hex=hex)), 
     tar_target(birdDat, 
               multi_taxa_hex_density(obs=obs, 
                                      transectsIn = studyTransects,
                                      taxaDf = allSpp)),
     tar_target(fullDf, 
                left_join(birdDat, traffDat)),
     tar_target(test,
                write.csv(fullDf,
                          file = "./data_processed/full_df.csv",
                          row.names = FALSE)))

mapped_pipeline <- tar_map(
  values = params,
  names = "name",
  tar_target(studyHexes, prep_region_hexes(study_area, hex)),
  tar_target(riskDfs, calculate_risk(df = fullDf, shpfile = studyHexes, region_name = name)))

# list(inits)
list(inits, mapped_pipeline)

# tar_validate() = make sure pipeline is ready to use
# tar_make() = run pipeline
