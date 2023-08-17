
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
save_combo_plot <- function(summCol, fallCol, taxaLab, plotName, region_name){
  
  comboplot <- ggarrange(summCol, fallCol, ncol=2, nrow=1, common.legend=TRUE, legend = "bottom")
  
  if(is.na(taxaLab)){ 
    comboplot <- comboplot + theme(panel.background = element_rect(fill = "white"))
  }else{
    comboplot <- annotate_figure(comboplot, top = text_grob(snake_to_label(taxaLab), face = "bold", size = 30)) +
      theme(panel.background = element_rect(fill = "white"))
  }
  
  if(!file.exists("./figures")){
    dir.create("./figures")
  }
  
  # Save figure
  ifelse(region_name == "aleut",
         ggsave(filename=plotName,
                plot= comboplot,
                width=12, height=4, units="in"),
         ggsave(filename=plotName,
                plot= comboplot,
                width=10, height=6, units="in"))
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
    geom_sf(data=basemap, fill="lightgray", lwd=0) +
    plottheme
  return(plt)
}


#' Define the plot theme.
#'
#' This function returns a predefined plot theme.
#'
#' @return A list of theme elements for the plot.
plot_theme <- function(){
  plottheme <- list(
    scale_x_longitude(ticks = 5, expand = c(0, 0)),
    scale_y_latitude(ticks = 5, expand = c(0, 0)),
    theme_bw(),
    theme(text = element_text(size = 18),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(size = 8, hjust=0),
          plot.margin = margin(t=0.5, r=0.5, b=0.5, l=0.5, unit="cm"),
          # panel.background = element_rect(fill = "#73b2ff"),
          panel.border =  element_rect(colour = "black"),
          axis.text = element_text(colour = "darkgray", size=8)))
  
  return(plottheme)
}


#' Generate a color palette.
#'
#' This function generates a color palette for plotting.
#'
#' @return A color palette.
generate_color_palette <- function(){
  cols <- c(colorRampPalette(c("#e7f0fa", "#c9e2f6", "#95cbee", "#0099dc", "#4ab04a", "#ffd73e"))(25),
            colorRampPalette(c("#eec73a", "#e29421", "#e29421", "#f05336","#ce472e"), bias=2)(25))
  return(cols)
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
  plotName <- paste0("./figures/traff_", nestDf$traff[1] ,"_", region_name, ".png")
  
  summCol <- traff_plot(nestDf$summer[[1]], region_name, hex, "Summer", basemap, box)
  fallCol <- traff_plot(nestDf$fall[[1]], region_name, hex, "Fall", basemap, box)
  
  save_combo_plot(summCol, fallCol, NA, plotName, region_name)
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
  
  cols <- generate_color_palette()
  
  dfSf <- hex %>% filter(hex_id %in% unique(df$hex_id)) %>% left_join(df, by="hex_id")
  
  pltEmpty <- plot_empty(basemap, box)
  plt <- pltEmpty  + 
    geom_sf(data=dfSf, aes(fill = traff_hrs), color="darkgray") +
    # scale_fill_steps(trans="log",n.breaks=4, low = "yellow", high = "red",nice.breaks=TRUE, labels=scales::comma,
    #                  name="Total Hours\nof Vessel Traffic", guide = guide_coloursteps(show.limits = TRUE)) +
    scale_fill_gradientn(colours=cols, trans="pseudo_log", breaks=br, labels=scales::label_comma(),name="Vessel Activity\n(Hours)", na.value="white", limits=lims) +
    # labs(caption = paste0("*One operating day is equal to one vessel present in a hex on a given day.")) + 
    guides(fill = guide_colourbar(barwidth = 25, 
                                  barheight = 1, 
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

  cols <- generate_color_palette()
  
  dfSf <- hex %>% filter(hex_id %in% unique(df$hex_id)) %>% left_join(df, by="hex_id")
  
  pltEmpty <- plot_empty(basemap, box)
  plt <- pltEmpty  + 
    geom_sf(data=filter(dfSf, density == 0), color="darkgray", fill=NA) +
    geom_sf(data=filter(dfSf, density != 0),aes(fill = density), color="darkgray") +
    scale_fill_gradientn(colours=cols, trans="log10", labels=scales::label_number(),name="Density \n(Ind'ls/km\u00b2)", na.value="white") +
    # scale_fill_steps(trans="log",low = "yellow", high = "red",nice.breaks=TRUE, labels=scales::label_number(), 
    #                  name="Density \n(Ind'ls/km\u00b2)", guide = guide_coloursteps(show.limits = TRUE)) + 
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    # labs(caption = paste0("*Empty hexes were surveyed, but no ", taxaLabel, " were sighted during study period.")) + 
    guides(fill = guide_colourbar(barwidth = 25, 
                                  barheight = 1, 
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
                                               region_name = region_name)})  
  
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
  
  plottheme <- plot_theme()
  
  dfSf <- hex %>% filter(hex_id %in% unique(df$hex_id)) %>% left_join(df, by="hex_id")
  
  pltEmpty <- plot_empty(basemap, box)
  plt <- pltEmpty +
    geom_sf(data=dfSf,aes(fill = risk_cat), color="darkgray") +
    scale_fill_manual(values = c("low" = "#73b2ff",
                                 "mediumBIRD" = "#90ee90",
                                 "mediumSHIP" = "#14ff96",
                                 "high" = "#f7f570",
                                 "veryhigh" = "#e31a1c"),
                      na.value = "white",
                      name="Risk", 
                      drop=F,
                      labels = c("Low", "Moderate (bird)", "Moderate (ship)", "High", "Very High")) +
    plottheme +
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
  
  plottheme <- plot_theme()
  
  dfSf <- hex %>% filter(hex_id %in% unique(df$hex_id)) %>% left_join(df, by="hex_id")
  
  cols <- generate_color_palette()
  
  pltEmpty <- plot_empty(basemap, box)
  plt <- pltEmpty +
    geom_sf(data=filter(dfSf, risk_cont == 0), color="darkgray", fill=NA) +
    geom_sf(data=filter(dfSf, risk_cont != 0),aes(fill = risk_cont), color="darkgray") +
    scale_fill_gradientn(colours=cols, labels=scales::label_number(),name="Risk Index", na.value="white") +
    # scale_fill_steps(trans="log",low = "yellow", high = "red",nice.breaks=TRUE, labels=scales::label_number(),
    #                  name="Density \n(Ind'ls/km\u00b2)", guide = guide_coloursteps(show.limits = TRUE)) +
    # labs(caption = paste0("*Empty hexes were surveyed, but no ", taxaLabel, " were sighted during study period.")) +
    guides(fill = guide_colourbar(barwidth = 25,
                                  barheight = 1,
                                  title.hjust = 0.5,
                                  ticks.colour="black",
                                  frame.colour = "black",
                                  ticks.linewidth = 0.75)) +
    plottheme +
    ggtitle(title)
  return(plt)
}


