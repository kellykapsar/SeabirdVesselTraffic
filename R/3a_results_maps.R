
#' Save a combined plot.
#'
#' This function creates and saves a combined plot of two seasonal subplots.
#'
#' @param summCol Summer subplot.
#' @param fallCol Fall subplot.
#' @param taxaLab Taxa label for the plot.
#' @param plotName Name of the saved plot file.
#' @param region_name Name of the region being analyzed.
#' @return None.
save_combo_plot <- function(summCol, fallCol, taxaLab, plotName, region_name, risk_cat = FALSE){
  
  if(risk_cat == FALSE){
    comboplot <- ggarrange(summCol, fallCol, ncol=2, nrow=1, common.legend=TRUE, legend = "right")
  }else{
    comboplot <- ggarrange(summCol, fallCol, ncol=2, nrow=1, legend = "none")
  }
  comboplot <- annotate_figure(comboplot, left = text_grob(snake_to_label(taxaLab), face = "bold", size = 30, rot=90)) +
      theme(panel.background = element_rect(fill = "white", color="white")) 
  
  if(!file.exists("./figures")){
    dir.create("./figures")
  }
  
  # Save figure
  ifelse(region_name == "aleut",
         ggsave(filename=plotName,
                plot= comboplot,
                width=12, height=3, units="in"),
         ggsave(filename=plotName,
                plot= comboplot,
                width=13, height=6, units="in"))
}


#' Create an empty plot.
#'
#' This function generates an empty plot with specified background elements.
#'
#' @param basemap Sf object containing basemap for a given region. 
#' @param box Bounding box object delineating boundaries of a given region.
#' @return The empty plot.
plot_empty <- function(basemap, box){
  
  plottheme <- plot_theme()
  
  plt <- ggplot() +
    geom_sf(data=box, fill=NA, color=NA,lwd=0) +   
    geom_sf(data=basemap, fill="lightgray", lwd=0, color=NA) +
    plottheme
  return(plt)
}


#' Define the plot theme.
#'
#' This function returns a predefined plot theme.
#'
#' @return A list of theme elements for the plot.
plot_theme <- function(map=TRUE){
  if(map == TRUE){
    plottheme <- list(
      scale_x_longitude(ticks = 5, expand = c(0, 0)),
      scale_y_latitude(ticks = 5, expand = c(0, 0)),
      theme_bw(),
      theme(text = element_text(size = 18),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(size = 8, hjust=0),
            plot.margin = margin(t=0, r=0, b=0, l=0, unit="mm"),
            # panel.background = element_rect(fill = "#333333"),
            panel.border =  element_rect(colour = "black"),
            axis.text = element_text(colour = "darkgray", size=8), 
            legend.position = "right", 
            legend.direction = "vertical"))
  }
  if(map == FALSE){
    plottheme <- list(
      theme_bw(),
      theme(text = element_text(size = 18),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(size = 8, hjust=0),
            plot.margin = margin(t=0.5, r=0.5, b=0.5, l=0.5, unit="cm"),
            # panel.background = element_rect(fill = "#333333"),
            panel.border =  element_rect(colour = "black"),
            axis.text = element_text(colour = "darkgray", size=8), 
            legend.position = "right", 
            legend.direction = "vertical"))
  }
  return(plottheme)
}

#' Plot vessel traffic.
#'
#' This function saves a seasonal vessel activity map a given region.
#'
#' @param nestDf Nested data frame containing seasonal risk data for each unique taxa and vessel activity metric combination.
#' @param region_name Name of the region being analyzed.
#' @param hex Hexagon data.
#' @param basemap Sf object containing basemap for a given region. 
#' @param box Bounding box object delineating boundaries of a given region.
#' @return None.
plot_traff <- function(nestDf, region_name, hex, basemap, box){
  
  nestDf$plotName <- paste0("./figures/traff_", nestDf$traff ,"_", region_name, ".png")
  
  # Remove rows with duplicate taxa, but differentvessel traffic rows
  nestDfnew <- nestDf[!duplicated(nestDf$traff),] 
  
  nestDfnew <- nestDfnew %>% mutate(summTraff = map(summer, ~traff_plot(.x, 
                                                                      region_name = region_name,
                                                                      hex = hex, 
                                                                      title = "Summer", 
                                                                      basemap = basemap, 
                                                                      box = box)), 
                                    fallTraff = map(fall, ~traff_plot(.x, 
                                                                    region_name = region_name,
                                                                    hex = hex, 
                                                                    title = "Fall", 
                                                                    basemap = basemap, 
                                                                    box = box)))
  
  apply(nestDfnew, 1, function(x){save_combo_plot(summCol = x$summTraff, 
                                                  fallCol = x$fallTraff, 
                                                  taxaLab = "Vessel Traffic",
                                                  plotName = x$plotName, 
                                                  region_name = region_name)})  
  
}


#' Plot vessel traffic.
#'
#' This function plots vessel traffic for a given data frame.
#'
#' @param df Individual risk data frame for one season and one bird density/vessel activity metric combo
#' @param region_name Name of the region being analyzed.
#' @param hex Hexagon data.
#' @param title Character string with title for plot.
#' @param basemap Sf object containing basemap for a given region. 
#' @param box Bounding box object delineating boundaries of a given region.
#' @return None.
traff_plot <- function(df, region_name, hex, title, basemap, box){

  if(region_name != "nbs-cs"){
    br <- c(0,10,100, 1000, 10000, 100000, 100000)
    lims <- c(0,150000)
  }else{
    br <- c(0,10,100, 1000, 10000)
    lims <- c(0,50000)
  }
  
  dfSf <- hex %>% filter(hex_id %in% unique(df$hex_id)) %>% left_join(df, by="hex_id")

  if(!file.exists(paste0("./data_processed/StudyAreaHexes_", region_name, ".shp"))){
    st_write(dfSf, paste0("./data_processed/StudyAreaHexes_", region_name, ".shp"))
  }
  
  pltEmpty <- plot_empty(basemap, box)
  plt <- pltEmpty  + 
    # geom_sf(data=dfSf, aes(fill = test), color="darkgray") +
    # https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
    # scale_fill_viridis_d(option = "H", direction = 1, name="Vessel Activity\n(Hours)") +
    
    geom_sf(data=dfSf, aes(fill = traff_hrs), color="darkgray") +
    scale_fill_viridis_c(option = "H", direction=1, trans="pseudo_log", breaks=br,
                         labels=scales::label_comma(),name="Vessel Activity\n(Hours)", na.value="white", limits=lims) +

    guides(fill = guide_colourbar(barwidth = 1, 
                                  barheight = 15, 
                                  title.hjust = 0.5,
                                  ticks.colour="black", 
                                  frame.colour = "black", 
                                  ticks.linewidth = 0.75)) +
    ggtitle(title)
  return(plt)
}


#' Plot seabird density.
#'
#' This function saves a seasonal seabird density map a given region.
#'
#' @param nestDf Nested data frame containing seasonal risk data for each unique taxa and vessel activity metric combination.
#' @param region_name Name of the region being analyzed.
#' @param hex Hexagon data.
#' @param basemap Sf object containing basemap for a given region. 
#' @param box Bounding box object delineating boundaries of a given region.
#' @return None.
plot_bird <- function(nestDf, region_name, hex, basemap, box){
  
  nestDf$plotName <- paste0("./figures/density_", nestDf$taxa ,"_", region_name, ".png")
  
  # Remove rows with duplicate taxa, but differentvessel traffic rows
  nestDfnew <- nestDf[!duplicated(nestDf$taxa),] 
  
  nestDfnew <- nestDfnew %>% mutate(summBird = map(summer, ~bird_plot(.x, 
                                                                   hex = hex, 
                                                                   title = "Summer", 
                                                                   basemap = basemap, 
                                                                   box = box)), 
                              fallBird = map(fall, ~bird_plot(.x, 
                                                                 hex = hex, 
                                                                 title = "Fall", 
                                                                 basemap = basemap, 
                                                                 box = box)))
  
  apply(nestDfnew, 1, function(x){save_combo_plot(summCol = x$summBird, 
                                               fallCol = x$fallBird, 
                                               taxaLab = x$taxa,
                                               plotName = x$plotName, 
                                               region_name = region_name)})  
}


#' Plot seabird density.
#'
#' This function plots seabird density for a given data frame.
#'
#' @param df Individual risk data frame for one season and one bird density/vessel activity metric combo
#' @param hex Hexagon data.
#' @param title Character string with title for plot.
#' @param basemap Sf object containing basemap for a given region. 
#' @param box Bounding box object delineating boundaries of a given region.
#' @return None.
bird_plot <- function(df, hex, title, basemap, box){

  dfSf <- hex %>% filter(hex_id %in% unique(df$hex_id)) %>% left_join(df, by="hex_id")
  
  pltEmpty <- plot_empty(basemap, box)
  plt <- pltEmpty  + 
    geom_sf(data=filter(dfSf, density == 0), color="darkgray", fill=NA) +
    geom_sf(data=filter(dfSf, density != 0),aes(fill = density), color="darkgray") +
    # scale_fill_gradientn(colours=cols, trans="log10", labels=scales::label_number(),name="Density \n(Ind'ls/km\u00b2)", na.value="white") +
    scale_fill_viridis_c(option = "H", direction=1, trans="log10", labels=scales::label_number(),name="Density \n(Ind'ls/km\u00b2)", na.value="white") +
    # labs(caption = paste0("*Empty hexes were surveyed, but no ", taxaLabel, " were sighted during study period.")) + 
    guides(fill = guide_colourbar(barwidth = 1, 
                                  barheight =15, 
                                  title.hjust = 0.5,
                                  ticks.colour="black", 
                                  frame.colour = "black", 
                                  ticks.linewidth = 0.75)) +
    ggtitle(title)
  return(plt)
}


#' Plot risk category.
#'
#' This function saves a series of seasonal categorical risk maps for all unique taxa and vessel activity metric combination.
#'
#' @param nestDf Nested data frame containing seasonal risk data nested within rows of each unique taxa and vessel activity metric combination..
#' @param region_name Name of the region being analyzed.
#' @param hex Hexagon data.
#' @param basemap Sf object containing basemap for a given region. 
#' @param box Bounding box object delineating boundaries of a given region.
#' @return None.
plot_risk_cat <- function(nestDf, region_name, hex, basemap, box){
  nestDf$plot_name <- paste0("./figures/risk-cat_", nestDf$traff ,"_", nestDf$taxa, "_", region_name, ".png")
  
  
  nestDf <- nestDf %>% mutate(summCat = map(summer, ~risk_cat_plot(.x, 
                                                                   hex = hex, 
                                                                   title = "Summer", 
                                                                   basemap = basemap, 
                                                                   box = box)), 
                              fallCat = map(fall, ~risk_cat_plot(.x, 
                                                                 hex = hex, 
                                                                 title = "Fall", 
                                                                 basemap = basemap, 
                                                                 box = box)))
  
  apply(nestDf, 1, function(x){save_combo_plot(summCol = x$summCat, 
                                               fallCol = x$fallCat, 
                                               taxaLab = x$taxa,
                                               plotName = x$plot_name, 
                                               region_name = region_name, 
                                               risk_cat = TRUE)})  
  
}


#' Plot risk category.
#'
#' This function creates a categorical risk map for a single taxa and vessel activity metric combination.
#'
#' @param df Individual risk data frame for one season and one bird density/vessel activity metric combo
#' @param hex Hexagon data.
#' @param title Title of the plot.
#' @param basemap Sf object containing basemap for a given region. 
#' @param box Bounding box object delineating boundaries of a given region.
#' @return The risk category plot.
risk_cat_plot <- function(df, hex, title, basemap, box){
  
  dfSf <- hex %>% filter(hex_id %in% unique(df$hex_id)) %>% left_join(df, by="hex_id")
  
  pltEmpty <- plot_empty(basemap, box)
  plt <- pltEmpty +
    geom_sf(data=dfSf,aes(fill = risk_cat), color="darkgray") +
    scale_fill_manual(values = c("low" = "#73b2ff",
                                 "mediumBIRD" = "#90ee90",
                                 "mediumSHIP" = "#14ff96",
                                 "high" = "#f7f570",
                                 "veryhigh" = "#e31a1c"),
    # scale_fill_manual(values = c("low" = "#60B257",
    #                              "mediumBIRD" = "#F1963F",
    #                              "mediumSHIP" = "#FADA4A",
    #                              "high" = "#EB3223",
    #                              "veryhigh" = "#911111"),
                      na.value = "white",
                      name="Risk", 
                      drop=F,
                      labels = c("Low", "Moderate (bird)", "Moderate (ship)", "High", "Very High")) +
    ggtitle(title)
  return(plt)
}


#' Plot continuous risk.
#'
#' This function saves a series of seasonal continuous risk maps for each unique taxa and vessel activity metric combination.
#'
#' @param nestDf Nested data frame containing seasonal risk data for each unique taxa and vessel activity metric combination.
#' @param region_name Name of the region being analyzed.
#' @param hex Hexagon data.
#' @param basemap Sf object containing basemap for a given region. 
#' @param box Bounding box object delineating boundaries of a given region.
#' @return None.
plot_risk_con <- function(nestDf, region_name, hex, basemap, box){
  nestDf$plot_name <- paste0("./figures/risk-con_", nestDf$traff ,"_", nestDf$taxa, "_", region_name, ".png")
  
  
  nestDf <- nestDf %>% mutate(summCat = map(summer, ~risk_con_plot(.x, 
                                                                   hex = hex, 
                                                                   title = "Summer", 
                                                                   basemap = basemap, 
                                                                   box = box)), 
                              fallCat = map(fall, ~risk_con_plot(.x, 
                                                                 hex = hex, 
                                                                 title = "Fall", 
                                                                 basemap = basemap, 
                                                                 box = box)))
  
  apply(nestDf, 1, function(x){save_combo_plot(summCol = x$summCat, 
                                               fallCol = x$fallCat, 
                                               taxaLab = x$taxa,
                                               plotName = x$plot_name, 
                                               region_name = region_name)})  
  
}


#' Plot continuous risk.
#'
#' This function creates a continuous risk map for a single taxa and vessel activity metric combination.
#'
#' @param df Individual risk data frame for one season and one bird density/vessel activity metric combo
#' @param hex Output of prep_regional_hexes. An sf object containing geometries of hexes with sufficient survey effort for inclusion in the study. 
#' @param title Title of the plot.
#' @param basemap Sf object containing basemap for a given region. 
#' @param box Bounding box object delineating boundaries of a given region.
#' @return The continuous risk plot.
risk_con_plot <- function(df, hex, title, basemap, box){
  
  dfSf <- hex %>% filter(hex_id %in% unique(df$hex_id)) %>% left_join(df, by="hex_id")
  
  pltEmpty <- plot_empty(basemap, box)
  plt <- pltEmpty +
    geom_sf(data=filter(dfSf, risk_cont == 0), color="darkgray", fill=NA) +
    geom_sf(data=filter(dfSf, risk_cont != 0),aes(fill = risk_cont), color="darkgray") +
    # scale_fill_gradientn(colours=cols, labels=scales::label_number(),name="Risk Index", na.value="white") +
    scale_fill_viridis_c(option = "H", direction=1, labels=scales::label_number(),name="Risk Index", na.value="white") +
    # scale_fill_steps(trans="log",low = "yellow", high = "red",nice.breaks=TRUE, labels=scales::label_number(),
    #                  name="Density \n(Ind'ls/km\u00b2)", guide = guide_coloursteps(show.limits = TRUE)) +
    # labs(caption = paste0("*Empty hexes were surveyed, but no ", taxaLabel, " were sighted during study period.")) +
    guides(fill = guide_colourbar(barwidth = 1,
                                  barheight = 15,
                                  title.hjust = 0.5,
                                  ticks.colour="black",
                                  frame.colour = "black",
                                  ticks.linewidth = 0.75)) +
    ggtitle(title)
  return(plt)
}


#' Create a joint plot of high-risk taxa counts.
#'
#' This function saves a map of the total number of high risk taxa groups per hex within a region.
#'
#' @param nestDf Nested data frame containing seasonal risk data for each unique taxa and vessel activity metric combination.
#' @param region_name Name of the region being analyzed.
#' @param hex Output of prep_regional_hexes. An sf object containing geometries of hexes with sufficient survey effort for inclusion in the study. 
#' @param basemap Sf object containing basemap for a given region. 
#' @param box Bounding box object delineating boundaries of a given region.
#' @return A joint plot of high-risk taxa counts.
plot_joint_high_risk <- function(nestDf, region_name, hex, basemap, box){
  summ <- format_risk_totals(nestDf, "summer")
  fall <- format_risk_totals(nestDf, "fall")
  
  summPlot <- joint_high_risk_plot(summ, hex, "Summer", basemap, box)
  fallPlot <- joint_high_risk_plot(fall, hex, "Fall", basemap, box)
  
  pltnm <- paste0("./figures/high_risk_taxa_counts_", region_name, ".png")
  
  save_combo_plot(summCol = summPlot, fallCol = fallPlot, taxaLab = NA, plotName = pltnm, region_name = region_name)
}


#' Format risk totals data for high-risk taxa counts.
#'
#' This function formats the nested risk data frames into counts of the number of high risk taxa groups per hex
#'
#' @param nestDf Nested data frame containing seasonal risk data for each unique taxa and vessel activity metric combination.
#' @param colname Name of the column.
#' @return Formatted data for high-risk taxa counts.
format_risk_totals <- function(nestDf, colname){
  
  t <- unnest(nestDf, cols = c({{colname}})) %>% 
    filter(traff == "hrs_al") %>% 
    filter(risk_cat %in% c("high", "veryhigh")) %>% 
    filter(taxa != "total_seabirds") %>% 
    group_by(hex_id) %>% 
    summarize(nTaxa = n())

return(t)
}


#' Create a map of the number of high-risk taxa groups
#'
#' This function generates a map of the number of high-risk taxa groups.
#'
#' @param df Data frame containing high-risk taxa counts created using the format_risk_totals function. 
#' @param hex Output of prep_regional_hexes. An sf object containing geometries of hexes with sufficient survey effort for inclusion in the study. 
#' @param title Title of the plot.
#' @param basemap Sf object containing basemap for a given region. 
#' @param box Bounding box object delineating boundaries of a given region.
#' @return A joint plot for high-risk taxa counts.
joint_high_risk_plot <- function(df, hex, title, basemap, box){
  
  dfSf <- left_join(hex, df, by="hex_id")
  dfSf$nTaxa[is.na(dfSf$nTaxa)] <- 0
  dfSf$nTaxa <- as.character(dfSf$nTaxa)
  
  pltEmpty <- plot_empty(basemap, box)
  
  plt <- pltEmpty +
    geom_sf(data=box, fill=NA, color=NA,lwd=0) +      
    geom_sf(data=basemap, fill="lightgray",lwd=0) +
    geom_sf(data=dfSf,aes(fill = nTaxa), color="darkgray") +
    scale_fill_manual(values = c("1" = "#ffae52",
                                 "2" = "#a83b00",
                                 "3" = "#281863",
                                 "4" = "black"),
                      na.value = "white",
                      name="Number of Taxa Groups", 
                      drop=F)
  return(plt)
}

