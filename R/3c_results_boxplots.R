
#' Box plot of percent high risk area faceted by region
#'
#' This function creates and saves a Box plot of percent high 
#' risk area for all seabird species faceted by region
#'
#' @param nestDf Nested data frame containing seasonal risk data for each unique taxa and vessel activity metric combination.
#' @param region_name Name of the region being analyzed.
#' @return None. Box plot saved in "./figures/"
boxplot_regions <- function(nestDf){
  
  t <- prep_boxplot(nestDf)
  
  tnew <- filter(t, taxa == "Total Seabirds")
  
  risk_boxplot_tod(df = tnew, facet_name = "region", region_name = NA)
}

#' Box plot of percent high risk area faceted by taxa
#'
#' This function creates and saves a Box plot of percent high 
#' risk area faceted for each taxa group.
#'
#' @param nestDf Nested data frame containing seasonal risk data for each unique taxa and vessel activity metric combination.
#' @param region_name Name of the region being analyzed.
#' @return None. Box plot saved in "./figures/"
boxplot_taxa <- function(nestDf, region_name){
  
  t <- prep_boxplot(nestDf)
  
  t$taxa <- as.factor(t$taxa)
  
  tnew <- t %>%  mutate(taxa = relevel(taxa, ref="Total Seabirds"))
  
  risk_boxplot_tod(df = tnew, facet_name = "taxa", region_name = region_name)
}

#' Prepare data for creating a Box plot.
#'
#' This function prepares data for creating a Box plot by reshaping and filtering.
#'
#' @param nestDf Nested data frame containing seasonal risk data for each unique taxa and vessel activity metric combination.
#' @param nightonly Logical indicating whether to consider nighttime traffic only.
#' @return Prepared data for creating a Box plot.
prep_boxplot <- function(nestDf, nightonly=FALSE){
  
    t <- nestDf %>% 
    filter(traff != "d_hrs_al") %>% 
    gather(key = season, value = risk, -taxa, -region, - traff) %>% 
    unnest(cols=c(risk)) %>% 
    mutate(taxa = as.factor(taxa), 
           region = as.factor(region), 
           traff = as.factor(traff), 
           season = as.factor(season))

  if(nightonly == TRUE){
    t <- t %>% filter(traff == "n_hrs_al")
  }
  
  t$season <- factor(t$season, levels = c("summer", "fall"), labels = c("Summer", "Fall"))
  
  t$taxa <- snake_to_label(t$taxa)
  
  return(t)
}

#' Create a Box plot for risk distribution by time of day.
#'
#' This function generates a Box plot illustrating the risk distribution by time of day.
#'
#' @param df Data frame output from prep_boxplot function
#' @param facet_name Name of column to input to facet_wrap.
#' @param region_name Name of the region being analyzed.
#' @return A Box plot for risk distribution by time of day.
risk_boxplot_tod <- function(df, facet_name, region_name){
  
  p <- ggplot(df, aes(x = season, y = risk_cont, fill = traff)) +
    geom_boxplot(position = "dodge") +
    scale_fill_manual(values=c("hrs_al" = "white",
                               "n_hrs_al" = "darkgray"),
                      labels = c("All", "Night Only"),
                      name="Vessel Traffic") +
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    facet_wrap(as.formula(paste("~", facet_name))) +
    ylab("Risk Index") + 
    xlab("") +
    theme_bw() + 
    theme(text = element_text(size=20))
  
  if(is.na(region_name)){
    filename <- paste0("./figures/risk_boxplot_by_", facet_name, "_total_seabirds.png")
  }else(
    filename <- paste0("./figures/risk_boxplot_by_", facet_name, "_", region_name, ".png")
  )
  
  ggsave(filename = filename,
         plot = p, width=12, height=8, units="in")
}

# FIX 
#' Create a Box plot for risk distribution during nighttime.
#'
#' This function generates a Box plot illustrating the risk distribution during nighttime.
#'
#' @param nestDf Nested data frame containing seasonal risk data for each unique taxa and vessel activity metric combination.
#' @return A Box plot for risk distribution during nighttime.
risk_boxplot_night <- function(nestDf){
  
  t <- prep_boxplot(nestDf, nightonly=TRUE)
  
  p <- ggplot(t, aes(x=season, y=risk_cont)) +
    geom_boxplot(position="dodge") +
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    facet_wrap(~region) + 
    ylab("Risk Index") + 
    xlab("") +
    theme_bw() + 
    theme(text = element_text(size=20))
  p
  ggsave(filename = paste0("./figures/risk_boxplot_total_seabirds_by_region_night.png"),
         plot = p, width=12, height=8, units="in")
}
