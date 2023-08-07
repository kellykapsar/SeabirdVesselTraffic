# tar_target(traffDat, 
# prep_traff(filedir = "D:/AIS_V2_DayNight_60km6hrgap/Hex_DayNight_Hours/")),
# tar_target(birdDat, multi_taxa_hex_density(obs=obs, 
#                                            transectsIn=studyTransects, 
#                                            taxaDf=allSpp, 
#                                            startYear = 2006)),
# tar_target(df, left_join(birdDat, traffDat)), 
# tar_target(output, write.csv(df, file = paste0("./data_processed/", name, ".csv")))



############
# fullArea <- hex[hex$hex_id %in% sharedIDs,]
# fullArea <- left_join(fullArea, st_drop_geometry(summLoc), by=c("hex_id", "area_km")) %>% 
#   rename(surv_eff_summ = survEff)
# fullArea <- left_join(fullArea, st_drop_geometry(fallLoc), by=c("hex_id", "area_km")) %>% 
#   rename(surv_eff_fall = survEff)
# 
# if(!is.na(studyArea)){
#   study <- st_read(paste0("./data_raw/study_areas/", studyArea), quiet = TRUE)
#   fullArea <- fullArea[st_intersects(fullArea, study, sparse=FALSE),] 
# }
# st_write(fullArea, paste0("./data_processed/study_area_hexes/", studyAreaName, "_hexes.shp"))