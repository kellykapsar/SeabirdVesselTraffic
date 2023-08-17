#' Create a summary table for region statistics.
#'
#' This function generates a summary table for region statistics based on provided data.
#'
#' @param fullDf Output from calculate_risk function. Data frame containing bird and vessel traffic data.  
#' @param nestDf f Nested data frame containing seasonal risk data for each unique taxa and vessel activity metric combination.
#' @param region_title Title of the region.
#' @return A summary table for region statistics.
region_summary_table <- function(fullDf, nestDf, region_title){
  subDf <- fullDf[fullDf$hex_id %in% nestDf$fall[[1]]$hex_id,]
  tb <- subDf %>% group_by(season) %>% summarize(n_hexes = length(unique(subDf$hex_id)),
                                                 sample_area = sum(sample_area),
                                                 n_transects = sum(n_transects),
                                                 hrs_al = sum(hrs_al),
                                                 n_hrs_al = sum(n_hrs_al),
                                                 d_hrs_al = sum(d_hrs_al))
  tb <- tb %>% mutate(region = region_title) %>% relocate(region)
  return(tb)
  # library(expss)
  # tb <- tb %>% apply_labels(sample_area = "Area surveyed (km**2)",
  #                           n_transects = "# of transects",
  #                           hrs_al = "Total vessel traffic (hours)", 
  #                           n_hrs_al = "Nighttime vessel traffic (hours)")
  # tb %>% tab_pivot()
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
           high_risk_summ = map_dbl(summer, ~sum(.$risk_cat %in% c("high", "veryhigh"))),
           high_risk_fall = map_dbl(fall, ~sum(.$risk_cat %in% c("high", "veryhigh"))),
           mean_risk_summ = round(map_dbl(summer, ~mean(.$risk_cont)),2),
           mean_risk_fall = round(map_dbl(fall, ~mean(.$risk_cont)),2),
           median_risk_summ = round(map_dbl(summer, ~median(.$risk_cont)),2),
           median_risk_fall = round(map_dbl(fall, ~median(.$risk_cont)),2),
           sd_risk_summ = round(map_dbl(summer, ~sd(.$risk_cont)),2),
           sd_risk_fall = round(map_dbl(fall, ~sd(.$risk_cont)),2),
           max_risk_summ = round(map_dbl(summer, ~max(.$risk_cont)),2),
           max_risk_fall = round(map_dbl(fall, ~max(.$risk_cont)),2)
    ) %>% 
    select(-fall, -summer)
  return(t)
}