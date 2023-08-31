#' Create a summary table for region statistics.
#'
#' This function generates a summary table for region statistics based on provided data.
#'
#' @param fullDf Output from calculate_risk function. Data frame containing bird and vessel traffic data.  
#' @param nestDf f Nested data frame containing seasonal risk data for each unique taxa and vessel activity metric combination.
#' @param region_title Title of the region.
#' @return A summary table for region statistics.
region_summary_table <- function(fullDf, nestDf, hex, region_title){
  
  subDf <- fullDf[fullDf$hex_id %in% nestDf$fall[[1]]$hex_id,]
  
  subDfnew <- st_drop_geometry(hex) %>% select(hex_id, area_km) %>% left_join(subDf, .)
  
  tb <- subDfnew %>% group_by(season) %>% summarize(study_area_size = round(sum(area_km), 0),
                                                 n_hexes = length(unique(hex_id)),
                                                 sample_area = round(sum(sample_area), 0),
                                                 n_transects = sum(n_transects),
                                                 hrs_al = round(sum(hrs_al), 0),
                                                 n_hrs_al = round(sum(n_hrs_al), 0),
                                                 d_hrs_al = round(sum(d_hrs_al), 0))
  tb <- tb %>% mutate(region = region_title) %>% relocate(region)
  
  return(tb)
  # library(expss)
  # tb <- tb %>% apply_labels(sample_area = "Area surveyed (km**2)",
  #                           n_transects = "# of transects",
  #                           hrs_al = "Total vessel traffic (hours)", 
  #                           n_hrs_al = "Nighttime vessel traffic (hours)")
  # tb %>% tab_pivot()
}

#' Region Summary Table 
#'
#' This function creates a table displaying summary data for vessel traffic and 
#' seabird observations fo eac region. NOTE: This function will need to be updated 
#' if regions or taxonomic groups are changed. 
#'
#' @param df Data frame containing risk information for all regions. Combined 
#' output of region_summary_table for all regions. 
#' @return Summary table of risk from all traffic. 
make_joint_region_table <- function(df){
  
  df$region <- factor(df$region, 
                      levels = c("All Alaska", 
                                 "Gulf of Alaska", 
                                 "Eastern Aleutians", 
                                 "Northern Bering & Chukchi Seas"))
  
  df$pct_night <- round(df$n_hrs_al/df$hrs_al*100, 0)
    
  n <- df %>%
    select(-d_hrs_al) %>% 
    gather(variable, value, -(region:n_hexes)) %>%
    unite(temp, season, variable) %>%
    spread(temp, value) %>% 
    arrange(region) %>% 
    select(region, 
           study_area_size,
           n_hexes, 
           summer_n_transects, 
           summer_sample_area, 
           summer_hrs_al, 
           summer_pct_night,
           fall_n_transects, 
           fall_sample_area, 
           fall_hrs_al, 
           fall_pct_night)
  
  write.csv(n, "./figures/region_summary_table.csv", row.names = FALSE)
  return(n)
}

#' Create a summary table for risk statistics.
#'
#' This function generates a summary table for risk statistics based on nested data.
#'
#' @param nestDf Nested data frame containing seasonal risk data for each unique taxa and vessel activity metric combination.
#' @param region_title Title of the region.
#' @return A summary table for risk statistics.
risk_summary_table <- function(nestDf, region_title){
  t <- nestDf %>% 
    mutate(region = region_title,
           pct_high_risk_summ = map_dbl(summer, ~round((sum(.$risk_cat %in% c("high", "veryhigh"))/length(.$risk_cat))*100, 2)),
           mean_risk_summ = round(map_dbl(summer, ~mean(.$risk_cont)),2),
           median_risk_summ = round(map_dbl(summer, ~median(.$risk_cont)),2),
           sd_risk_summ = round(map_dbl(summer, ~sd(.$risk_cont)),2),
           max_risk_summ = round(map_dbl(summer, ~max(.$risk_cont)),2),
           pct_high_risk_fall = map_dbl(fall, ~round((sum(.$risk_cat %in% c("high", "veryhigh"))/length(.$risk_cat))*100, 2)),
           mean_risk_fall = round(map_dbl(fall, ~mean(.$risk_cont)),2),
           median_risk_fall = round(map_dbl(fall, ~median(.$risk_cont)),2),
           sd_risk_fall = round(map_dbl(fall, ~sd(.$risk_cont)),2),
           max_risk_fall = round(map_dbl(fall, ~max(.$risk_cont)),2)
    ) %>% 
    select(-fall, -summer)
  return(t)
}

#' Risk Summary Table 
#'
#' This function creates a table displaying summary data for risk by 
#' taxa, region, and season. NOTE: This function will need to be updated if 
#' regions or taxonomic groups are changed. 
#'
#' @param df Data frame containing risk information for all regions. Combined 
#' output of risk_summary_table for all regions. 
#' @return Summary table of risk from all traffic. 
make_joint_risk_table <- function(df){
  
  df$region <- factor(df$region, 
                      levels = c("All Alaska", 
                                 "Gulf of Alaska", 
                                 "Eastern Aleutians", 
                                 "Northern Bering & Chukchi Seas"))
  
  n <- df %>% 
    ungroup() %>% 
    filter(traff == "hrs_al") %>%
    mutate(taxa = factor(snake_to_label(taxa))) %>% 
    mutate(taxa = relevel(taxa, ref="Total Seabirds")) %>% 
    arrange(taxa, region) %>% 
    select(-traff)
  
  write.csv(n, "./figures/risk_summary_table.csv", row.names = FALSE)
  return(n)
}
