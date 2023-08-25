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
           summer_n_hrs_al,
           fall_n_transects, 
           fall_sample_area, 
           fall_hrs_al, 
           fall_n_hrs_al)
  
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

#' Bar graph of percent high risk area faceted by region
#'
#' This function creates and saves a bar graph of percent high 
#' risk area for all seabird species faceted by region
#'
#' @param df Data frame containing risk information for all regions. 
#' @param region_name Name of the region being analyzed.
#' @return None. Bar graph saved in "./figures/"
bar_graph_regions <- function(df){
  t <- prep_bar_graph(df)
  
  tnew <- filter(t, taxa == "Total Seabirds")
  
  risk_bar_graph_tod(df = t, facet_name = "region", region_name = NA)
}

#' Bar graph of percent high risk area faceted by taxa
#'
#' This function creates and saves a bar graph of percent high 
#' risk area faceted for each taxa group.
#'
#' @param df Data frame containing risk information for one region 
#' @param region_name Name of the region being analyzed.
#' @return None. Bar graph saved in "./figures/"
bar_graph_taxa <- function(df, region_name){
  t <- prep_bar_graph(df)
  
  t$taxa <- as.factor(t$taxa)
  
  t <- t %>%  mutate(taxa = relevel(taxa, ref="Total Seabirds"))
  
  risk_bar_graph_tod(df = t, facet_name = "taxa", region_name = region_name)
}
  
#' Prepare data for creating a bar graph.
#'
#' This function prepares data for creating a bar graph by reshaping and filtering.
#'
#' @param df Data frame containing risk information.
#' @param nightonly Logical indicating whether to consider nighttime traffic only.
#' @return Prepared data for creating a bar graph.
prep_bar_graph <- function(df, nightonly=FALSE){
  
  t <-df %>% 
    select(pct_high_risk_summ, pct_high_risk_fall, taxa, region, traff) %>% 
    gather(key=subset,value=riskpct, -taxa, -region, -traff) %>% 
    filter(traff != "d_hrs_al")
  
  if(nightonly == TRUE){
    t <- t %>% filter(traff == "n_hrs_al")
  }
  
  test <- strsplit(t$subset,split = "_")
  t$season <- sapply(test, "[[", 4)
  t$season <- factor(t$season, levels = c("summ", "fall"), labels = c("Summer", "Fall"))
  
  t$taxa <- snake_to_label(t$taxa)

  return(t)
}

#' Create a bar graph for risk distribution by time of day.
#'
#' This function generates a bar graph illustrating the risk distribution by time of day.
#'
#' @param df Data frame containing risk information.
#' @param facet_name Name of column to input to facet_wrap.
#' @param region_name Name of the region being analyzed.
#' @return A bar graph for risk distribution by time of day.
risk_bar_graph_tod <- function(df, facet_name, region_name){
  
  p <- ggplot(df, aes(x=season, y=riskpct, fill=traff)) +
    geom_bar(position="dodge", stat="identity") +
    scale_fill_manual(values=c("hrs_al" = "#fee227",
                               "n_hrs_al" = "#191970"),
                      labels = c("All", "Night Only"),
                      name="Vessel Traffic") +
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    facet_wrap(as.formula(paste("~", facet_name))) +
    ylab("Percent of Study Area at High or Very High Risk") + 
    xlab("") +
    theme_bw() + 
    theme(text = element_text(size=20))
    
  if(is.na(region_name)){
    filename <- paste0("./figures/risk_bar_graph_by_", facet_name, "_total_seabirds.png")
  }else(
    filename <- paste0("./figures/risk_bar_graph_by_", facet_name, "_", region_name, ".png")
  )

  ggsave(filename = filename,
         plot = p, width=12, height=8, units="in")
}

# FIX 
#' Create a bar graph for risk distribution during nighttime.
#'
#' This function generates a bar graph illustrating the risk distribution during nighttime.
#'
#' @param df Data frame containing risk information.
#' @return A bar graph for risk distribution during nighttime.
risk_bar_graph_night <- function(df){
  
  t <- prep_bar_graph(df, nightonly=TRUE)
  
  p <- ggplot(t, aes(x=season, y=riskpct)) +
    geom_bar(position="dodge", stat="identity") +
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    facet_wrap(~region) + 
    ylab("Percent of Study Area at High or Very High Risk") + 
    xlab("") +
    theme_bw() + 
    theme(text = element_text(size=20))
  p
  ggsave(filename = paste0("./figures/risk_bar_graph_total_seabirds_by_region_night.png"),
         plot = p, width=12, height=8, units="in")
}
