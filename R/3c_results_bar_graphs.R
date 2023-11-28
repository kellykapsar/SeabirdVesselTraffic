
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
  
  risk_bar_graph_tod(df = tnew, facet_name = "region", region_name = NA)
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
    select(mean_risk_summ, mean_risk_fall, taxa, region, traff) %>% 
    gather(key=subset,value=risk_val, -taxa, -region, -traff)  %>% 
    filter(traff != "d_hrs_al")
  
  test <- strsplit(t$subset,split = "_")
  t$season <- sapply(test, "[[", 3)
  t$season <- factor(t$season, levels = c("summ", "fall"), labels = c("Summer", "Fall"))
  
  t2 <-df %>% 
    select(sd_risk_summ, sd_risk_fall, taxa, region, traff) %>% 
    gather(key=subset,value=risk_sd, -taxa, -region, -traff)  %>% 
    filter(traff != "d_hrs_al")
  
  test2 <- strsplit(t2$subset,split = "_")
  t2$season <- sapply(test2, "[[", 3)
  t2$season <- factor(t2$season, levels = c("summ", "fall"), labels = c("Summer", "Fall"))
  
  t <- left_join(t, t2, by = c("taxa", "region", "traff", "season")) %>% select(-subset.x, -subset.y)
  
  if(nightonly == TRUE){
    t <- t %>% filter(traff == "n_hrs_al")
  }
  
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
  
  p <- ggplot(df, aes(x=season, y=risk_val, fill=traff)) +
    geom_bar(position="dodge", stat="identity") +
    geom_errorbar(aes(ymin=risk_val-risk_sd, ymax=risk_val+risk_sd), width=.2,
                  position=position_dodge(.9)) +
    scale_fill_manual(values=c("hrs_al" = "lightgray",
                               "n_hrs_al" = "darkgray"),
                      labels = c("All", "Night Only"),
                      name="Vessel Traffic") +
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    facet_wrap(as.formula(paste("~", facet_name))) +
    ylab("Mean Risk Index") + 
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
  
  t <- prep_bar_graph(df, nightonly=TRUE) %>% filter(taxa == "Total Seabirds")
  
  p <- ggplot(t, aes(x=season, y=risk_val)) +
    geom_bar(position="dodge", stat="identity") +
    geom_errorbar(aes(ymin=risk_val-risk_sd, ymax=risk_val+risk_sd), width=.2,
                  position=position_dodge(.9)) +
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    facet_wrap(~region) + 
    ylab("Mean Risk Index") + 
    xlab("") +
    theme_bw() + 
    theme(text = element_text(size=20))
  p
  ggsave(filename = paste0("./figures/risk_bar_graph_total_seabirds_by_region_night.png"),
         plot = p, width=12, height=8, units="in")
}