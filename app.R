#shiny ui for viewing modeling results
#===================================================================================#
#####INSTALL PACKAGES#####
#install.packages('shiny')
library(shiny)
#install.packages('shinythemes')
library(shinythemes)
#install.packages('shinyWidgets')
library(shinyWidgets)
#install.packages('tidyverse')
library(tidyverse)
#install.packages('dplyr')
library(dplyr)
#install.packages('leaflet')
library(leaflet)
#install.packages('foreign')
library(foreign)
#install.packages('sp')
library(sp)
#install.packages('readr')
library(readr)
#install.packages('dygraphs')
library(dygraphs)
#install.packages('xts')
library(xts)
#install.packages('Hmisc')
library(Hmisc)
#####
#===================================================================================#


#===================================================================================#
#####READ THE DATA IN####
#read in site locations
site_loc <- read.csv('./output_data/site_locations.csv')
site_loc$STAID <- str_pad(site_loc$STAID, 8, pad = "0")


shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      h4('Site Locations'),
      selectInput("sitename", label = h4("Select site"), selected = 'Raccoon River at Van Meter, IA', choices = c(site_loc$STANAME)),
      leafletOutput('map'),
      selectInput("peformance_metric_map", label = h4("Select Performance Metric to Display on Map"), selected = "RMSE_Valid", choices = c('RMSE_Valid','MI_Valid','NSE_Valid','RMSE_Calib','MI_Calib','NSE_Calib')),
      hr(),
      h3('Explanation'),
      p("This is an application for comparing models of in-stream nitrate concentration for 29 sites shown above. All models were developed using an LSTM structure with a sequence length of 150 days, 20 cells, and a learning rate of 0.001. The time series shows the model performance over the holdout validation period which comprised 25% of each sites' record. Below are details on each model displayed here."),
      p(span(strong('local: '),style = 'color:#f94144'),em("features: TempMin, TempMax, Precip, Discharge -- "),"model trained on each individual site and tested at that site"), 
      p(span(strong('local_bfs:'),style = 'color:#f8961e'),em("features: TempMin, TempMax, Precip, Quickflow, Baseflow -- "), "model trained on each individual site and tested at that site, baseflow separation was performed on discharge to generate 'quickflow' time series and 'baseflow' time series"),
      p(span(strong('all_ws:'),style = 'color:#772e25'),em("features: TempMin, TempMax, Precip, Dishcharge -- "),"model trained on aggregated data from all watersheds and tested at each individual site"),
      p(span(strong('all_ws_bfs:'),style = 'color:#2b9348'), em("features: TempMin, TempMax, Precip, Quickflow, Baseflow --"),"model trained on aggregated data from all watersheds and tested at each individual site, baseflow separation was performed on discharge to generate 'quickflow' time series and 'baseflow' time series"),
      p(span(strong('all_ws_attr:'),style = 'color:#0096c7'),em("features: TempMin, TempMax, Precip, Dishcharge, Watershed Attributes --"), "model trained on aggregated data from all watersheds and tested at each individual site, watershed static attributes from GAGESII dataset were added (96 attributes including land use, topography, watershed size, and hydrologic connections)"),
      p(span(strong('all_ws_bfs_attr:'),style = 'color:cyan'),em("features: TempMin, TempMax, Precip, Dishcharge, Watershed Attributes --"), "model trained on aggregated data from all watersheds and tested at each individual site, watershed static attributes from GAGESII dataset were added (96 attributes including land use, topography, watershed size, and hydrologic connections), baseflow separation was performed on discharge to generate 'quickflow' time series and 'baseflow' time series")
    ),
    mainPanel(
      fluidRow(
        column(8,
               dygraphOutput("valid_plot")),
        column(2,
               uiOutput('legend')),
        column(2,
               checkboxGroupInput('model_display', label = h4("Select which models to display"), choices = c('Observed', 'local','local_bfs','all_ws','all_ws_bfs','all_ws_attr','all_ws_bfs_attr'), selected = c('Observed','local','all_ws','all_ws_bfs_attr')))
      ),
      fluidRow(
        column(4,
               selectInput("peformance_metric", label = h4("Select Performance Metric"), selected = "RMSE_Valid", choices = c('RMSE_Valid','MI_Valid','NSE_Valid','RMSE_Calib','MI_Calib','NSE_Calib'))),
        column(4,
               plotOutput('model_metrics_plot')),
        column(4,
               tableOutput('metrics_table'))
      )
    )
  )
)
)

#shiny server for looking at modeling results

function(input, output, session) {
  
  #---------------------------------------------------------------#
  #####MAP OF SITES######
  output$map <- renderLeaflet({
    model_metrics_map <- read.csv('./output_data/model_metrics_new.csv')
    model_metrics_map$Site <- str_pad(model_metrics_map$Site, width = 8, pad = '0')
    best_models <- data.frame()
    
    metric <- input$peformance_metric_map
    
    for(i in 1:length(unique(model_metrics_map$Site))){
      site_temp <- model_metrics_map[model_metrics_map$Site == unique(model_metrics_map$Site)[i],]
      if(grepl('RMSE',metric)|grepl('NSE',metric)){
        best_models <- rbind(best_models, site_temp[site_temp[metric] == min(site_temp[metric]),c("Site","Model")])
      }else{
        best_models <- rbind(best_models, site_temp[site_temp[metric] == max(site_temp[metric]),c("Site","Model")])
      }
    }
    
    #####
    #read in the site locations
    site_loc <- read.csv('./output_data/site_locations.csv')
    site_loc$STAID <- str_pad(site_loc$STAID, 8, pad = "0")
    
    #merge site locations with best models
    site_loc_df <- merge(site_loc, best_models, by.x = 'STAID', by.y = 'Site')
    
    #select the single site of interest
    single_site <- site_loc[site_loc$STANAME == input$sitename,]
    single_site_df <- as.data.frame(single_site)
    #make colors for site_loc_df
    site_loc_df$colors <- NA
    site_loc_df[site_loc_df$Model == 'local','colors'] <- '#f94144'
    site_loc_df[site_loc_df$Model == 'local_bfs','colors'] <- '#f8961e'
    site_loc_df[site_loc_df$Model == 'all_ws','colors'] <- '#772e25'
    site_loc_df[site_loc_df$Model == 'all_ws_bfs','colors'] <- '#2b9348'
    site_loc_df[site_loc_df$Model == 'all_ws_attr','colors'] <- '#0096c7'
    site_loc_df[site_loc_df$Model == 'all_ws_bfs_attr','colors'] <- 'cyan'
    
    
    
    single_site_loc <- SpatialPointsDataFrame(coords = single_site[,2:3], data = single_site_df, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    
    pal <- colorFactor(c('#f94144','#f8961e','#772e25','#2b9348','#0096c7','cyan'), levels = c("local", "local_bfs","all_ws","all_ws_bfs","all_ws_attr","all_ws_bfs_attr"), ordered = FALSE)
    
    leaflet(site_loc_df) %>% addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addCircleMarkers(lng = ~LNG_GAGE, lat = ~LAT_GAGE,
                       fillColor = ~colors, stroke = T, fill = T, weight = 1, opacity = 1, fillOpacity = 1,
                       radius = 6, color = 'black', label = ~STANAME) %>%
      leaflet::addLegend("topright", colors = c('#f94144','#f8961e','#772e25','#2b9348','#0096c7','cyan'), 
                         labels = c("local", "local_bfs","all_ws","all_ws_bfs","all_ws_attr","all_ws_bfs_attr"),
                         title = "Best Performing Model Nitrate <br/> Mutual Information Validation",
                         opacity = 1) %>% 
      addCircleMarkers(lng = as.numeric(single_site_loc$LNG_GAGE), lat = as.numeric(single_site_loc$LAT_GAGE),
                       popup = single_site_loc$STANAME, radius = 12, color = 'black', stroke = T, fill = T, weight = 2, opacity = 1, fillOpacity = 0)
    
    
    
  })
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####Validation Time Series######
  
  output$valid_plot <- renderDygraph({
    #read in site locations
    site_loc <- read.csv('./output_data/site_locations.csv')
    site_loc$STAID <- str_pad(site_loc$STAID, 8, pad = "0")
    #read in model results
    model_results <- list()
    for(i in 1:length(site_loc$STAID)){
      model_results[[site_loc$STAID[i]]] <- read_csv(paste('./output_data/', site_loc$STAID[i],'.csv', sep = ''), col_types = 'Dfnnnnnnn')
    }
    
    site_no <- site_loc[site_loc$STANAME == input$sitename,]$STAID
    plot_site <- model_results[[site_no]][,c(3:9)]
    
    datetime_plotting <- model_results[[site_no]]['DateTime']
    
    plot_site[,c(which(colnames(plot_site) %nin% input$model_display))] <- NA
    
    don_obs <- xts(x = plot_site, order.by = datetime_plotting$DateTime)
    
    dygraph(don_obs, main = paste0('Comparison of model results for validation period at ',input$sitename), ylab = 'Nitrate (mg/L)') %>%
      dySeries('Observed', strokeWidth = 3, color = 'darkgray') %>%
      dySeries('local', strokeWidth = 2, color = '#f94144', strokePattern = 'dashed') %>%
      dySeries('local_bfs', strokeWidth = 2, color = '#f8961e', strokePattern = 'dashed') %>%
      dySeries('all_ws', strokeWidth = 2, color = '#772e25') %>%
      dySeries('all_ws_bfs', strokeWidth = 2, color = '#2b9348') %>%
      dySeries('all_ws_attr', strokeWidth = 2, color = '#0096c7') %>%
      dySeries('all_ws_bfs_attr', strokeWidth = 2, color = 'cyan') %>%
      dyRangeSelector() %>%
      dyLegend(labelsSeparateLines = TRUE, labelsDiv = 'div_legend', show = 'always')
  })
  output$legend <- renderUI({
    htmlOutput("div_legend", height = "400px")
  })
  
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####BoxPlot of Model Metrics######
  
  output$model_metrics_plot <- renderPlot({
    site_loc <- read.csv('./output_data/site_locations.csv')
    site_loc$STAID <- str_pad(site_loc$STAID, 8, pad = "0")
    
    site_no <- site_loc[site_loc$STANAME == input$sitename,]$STAID
    
    #read in model metrics
    model_metrics <- read.csv('output_data/model_metrics_new.csv')
    model_metrics$Site <- str_pad(model_metrics$Site, 8, pad = "0")
    
    plot_bar <- model_metrics %>%
      filter(Site == site_no) %>%
      select(input$peformance_metric,"Model")
    metric_plot <- c(plot_bar[plot_bar$Model == 'local',][,1], plot_bar[plot_bar$Model == 'local_bfs',][,1],plot_bar[plot_bar$Model == 'all_ws',][,1],plot_bar[plot_bar$Model == 'all_ws_bfs',][,1],plot_bar[plot_bar$Model == 'all_ws_attr',][,1],plot_bar[plot_bar$Model == 'all_ws_bfs_attr',][,1])
    par(xpd = FALSE, mar = c(14,10,1,0))
    barplot(metric_plot, names.arg = c('local','local_bfs','all_ws','all_ws_bfs','all_ws_attr','all_ws_bfs_attr'),
            col = c('#f94144','#f8961e','#772e25','#2b9348','#0096c7','cyan'), ylab = input$performance_metric,
            horiz = TRUE, las = 1, cex.names = 1.5, cex.axis = 1.5)
    mtext(input$peformance_metric, side = 1, line = 3, cex = 1.5)
    text(y = seq(0.75,7,1.2),x = min(metric_plot)*0.25,labels=as.character(round(metric_plot, digits = 2)))
    
  })
  
}



shinyApp(ui = ui, server = server)