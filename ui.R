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



