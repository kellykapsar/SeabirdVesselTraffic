plot_risk_categorical <- function(nestdf, region_name, hex, basemap){
  riskplotallname <- paste0("./figures/risk-cat_", nestDf$traff ,"_", nestDf$taxa, "_", region_name, ".png")
  
  basemapnew <- basemap %>% st_crop(st_buffer(st_as_sfc(st_bbox(filtdf)), 10000))
  
  box <- st_as_sf(name="boundary", st_buffer(st_as_sfc(st_bbox(filtdf)), 10000))
  
    dfsub <- dfnew %>% filter(season == seasons[j])
    dfsub$riskcat <- factor(dfsub$riskcat, 
                            levels = c("low", "mediumBIRD", "mediumSHIP", "high", "veryhigh"))

    
    assign(paste0("p", j), plt)
    
  comboplot <- ggarrange(p1, p2, ncol=2, nrow=1, common.legend=TRUE, legend = "bottom")
  comboplot <- annotate_figure(comboplot, top = text_grob(dfsub$taxa[1], face = "bold", size = 30)) + 
    theme(panel.background = element_rect(fill = "white"))
  
  # Save figure
  if(!file.exists(riskplotallname)){
    ifelse(studyareaname == "Eastern Aleutians",
           ggsave(filename=riskplotallname,
                  plot= comboplot,
                  width=12, height=4, units="in"),
           ggsave(filename=riskplotallname,
                  plot= comboplot,
                  width=10, height=6, units="in"))
  }
}

plot_risk <- function(df, colName, title, basemap){
  
  plottheme <- plot_theme()
  
  plt <- ggplot() +
    geom_sf(data=box, fill=NA, color=NA,lwd=0) +      
    geom_sf(data=basemapnew, fill="lightgray",lwd=0) +
    geom_sf(data=dfsub,aes(fill = riskcat), color="darkgray") +
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
    ggtitle(paste0(dfsub$season[1]))
}

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

