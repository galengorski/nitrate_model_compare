#shiny server for looking at modeling results

function(input, output, session) {
  
  #---------------------------------------------------------------#
  #####MAP OF SITES######
  output$map <- renderLeaflet({
    model_metrics_map <- read_csv('./output_data/model_metrics_w_colors.csv', show_col_types = FALSE)
  
    metric <- input$peformance_metric_map
    
    best_models <- model_metrics_map %>%
      select(Model, Site, metric, colors) %>%
      mutate(Perf_Metric = ifelse(grepl('PBIAS',metric),abs(model_metrics_map[metric]), 
                                  model_metrics_map[metric])[[1]]) %>%
      group_by(Site) %>%
      arrange(desc(Perf_Metric), .by_group = TRUE) %>%
      summarise(Model = if_else(grepl('RMSE',metric)|grepl('PBIAS',metric), last(Model), first(Model)), 
                Site = if_else(grepl('RMSE',metric)|grepl('PBIAS',metric), last(Site), first(Site)), 
                Metric = if_else(grepl('RMSE',metric)|grepl('PBIAS',metric), last(Perf_Metric), first(Perf_Metric)),
                Color = if_else(grepl('RMSE',metric)|grepl('PBIAS',metric), last(colors), first(colors)))
    
    
    #####
    #read in the site locations
    site_loc <- read_csv('./output_data/site_locations.csv', show_col_types = FALSE)
    
    #merge site locations with best models
    site_loc_df <- merge(site_loc, best_models, by.x = 'STAID', by.y = 'Site')
    
    #select the single site of interest
    single_site <- site_loc[site_loc$STANAME == input$sitename,]
    single_site_df <- as.data.frame(single_site)
    
    leaflet(site_loc_df) %>% addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addCircleMarkers(lng = ~LNG_GAGE, lat = ~LAT_GAGE,
                       fillColor = ~Color, stroke = T, fill = T, weight = 1, opacity = 1, fillOpacity = 1,
                       radius = 6, color = 'black', label = paste(site_loc_df$STANAME, paste(input$peformance_metric_map, round(site_loc_df$Metric, 3),sep = '='), sep = ' | \n')) %>%
      leaflet::addLegend("topright", colors = c('#f94144','#f8961e','#772e25','#2b9348','#0096c7','cyan'), 
                         labels = c("local", "local_bfs","all_ws","all_ws_bfs","all_ws_attr","all_ws_bfs_attr"),
                         title = paste0("Best Performing Model Nitrate <br/>", input$peformance_metric_map),
                         opacity = 1) %>% 
      addCircleMarkers(lng = as.numeric(single_site_df$LNG_GAGE), lat = as.numeric(single_site_df$LAT_GAGE),
                        popup = single_site_df$STANAME, radius = 12, color = 'black', stroke = T, fill = T, weight = 2, opacity = 1, fillOpacity = 0)
      
        
    
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
      model_results[[site_loc$STAID[i]]] <- read_csv(paste('./output_data_ds/', site_loc$STAID[i],'.csv', sep = ''), col_types = 'Dfnnnnnnn')
    }
    
    site_no <- site_loc[site_loc$STANAME == input$sitename,]$STAID
    #head(model_results[[site_no]])
    plot_site <- model_results[[site_no]][,c(9:15)]
    
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

  # output$model_metrics_plot <- renderPlot({
  #   site_loc <- read.csv('./output_data/site_locations.csv')
  #   site_loc$STAID <- str_pad(site_loc$STAID, 8, pad = "0")
  #   
  #   site_no <- site_loc[site_loc$STANAME == input$sitename,]$STAID
  #   
  #   #read in model metrics
  #   model_metrics <- read.csv('output_data/model_metrics_w_colors.csv')
  #   model_metrics$Site <- str_pad(model_metrics$Site, 8, pad = "0")
  #   
  #   plot_bar <- model_metrics %>%
  #     filter(Site == site_no) %>%
  #     select(input$peformance_metric,"Model")
  #   metric_plot <- c(plot_bar[plot_bar$Model == 'local',][,1], plot_bar[plot_bar$Model == 'local_bfs',][,1],plot_bar[plot_bar$Model == 'all_ws',][,1],plot_bar[plot_bar$Model == 'all_ws_bfs',][,1],plot_bar[plot_bar$Model == 'all_ws_attr',][,1],plot_bar[plot_bar$Model == 'all_ws_bfs_attr',][,1])
  #   par(xpd = FALSE, mar = c(14,10,1,0))
  #   barplot(metric_plot, names.arg = c('local','local_bfs','all_ws','all_ws_bfs','all_ws_attr','all_ws_bfs_attr'),
  #           col = c('#f94144','#f8961e','#772e25','#2b9348','#0096c7','cyan'), ylab = input$performance_metric,
  #           horiz = TRUE, las = 1, cex.names = 1.5, cex.axis = 1.5)
  #   mtext(input$peformance_metric, side = 1, line = 3, cex = 1.5)
  #   text(y = seq(0.75,7,1.2),x = min(metric_plot)*0.25,labels=as.character(round(metric_plot, digits = 2)))
  # 
  # })
  # #---------------------------------------------------------------#
  #####BoxPlot of Model Metrics 2######

  output$model_metrics_plot_2 <- renderPlot({
    site_loc <- read.csv('./output_data/site_locations.csv')
    site_loc$STAID <- str_pad(site_loc$STAID, 8, pad = "0")

    site_no <- site_loc[site_loc$STANAME == input$sitename,]$STAID

    #read in model metrics
    model_metrics <- read.csv('output_data/model_metrics_w_colors.csv')
    model_metrics$Site <- str_pad(model_metrics$Site, 8, pad = "0")

    plot_bar <- model_metrics %>%
      filter(Site == site_no) %>%
      select(input$peformance_metric_2,"Model")
    metric_plot <- c(plot_bar[plot_bar$Model == 'local',][,1], plot_bar[plot_bar$Model == 'local_bfs',][,1],plot_bar[plot_bar$Model == 'all_ws',][,1],plot_bar[plot_bar$Model == 'all_ws_bfs',][,1],plot_bar[plot_bar$Model == 'all_ws_attr',][,1],plot_bar[plot_bar$Model == 'all_ws_bfs_attr',][,1])
    par(xpd = FALSE, mar = c(14,10,1,0))
    barplot(metric_plot, names.arg = c('local','local_bfs','all_ws','all_ws_bfs','all_ws_attr','all_ws_bfs_attr'),
            col = c('#f94144','#f8961e','#772e25','#2b9348','#0096c7','cyan'), ylab = input$performance_metric_2,
            horiz = TRUE, las = 1, cex.names = 1.5, cex.axis = 1.5)
    mtext(input$peformance_metric_2, side = 1, line = 3, cex = 1.5)
    text(y = seq(0.75,7,1.2),x = min(metric_plot)*0.25,labels=as.character(round(metric_plot, digits = 2)))

  })

  #---------------------------------------------------------------#
  #####Validation Time Series######
  
  output$pred_plot <- renderDygraph({
    #read in site locations
    site_loc <- read.csv('./output_data/site_locations.csv')
    site_loc$STAID <- str_pad(site_loc$STAID, 8, pad = "0")
    #read in model results
    model_results <- list()
    for(i in 1:length(site_loc$STAID)){
      model_results[[site_loc$STAID[i]]] <- read_csv(paste('./output_data_ds/', site_loc$STAID[i],'.csv', sep = ''), col_types = 'Dfnnnnnnn')
    }
    
    site_no <- site_loc[site_loc$STANAME == input$sitename,]$STAID
    plot_site <- model_results[[site_no]][,c(3:5,8)]
    
    plot_site <- plot_site %>%
      mutate(precip = precip/10)
    
    #print(head(model_results[[site_no]]))
    
    datetime_plotting <- model_results[[site_no]]['DateTime']
    
    plot_site[,c(which(colnames(plot_site) %nin% input$pred_display))] <- NA
    
    don_obs <- xts(x = plot_site, order.by = datetime_plotting$DateTime)
    
    dygraph(don_obs, main = paste0('Features used for prediction at ',input$sitename), ylab = 'Discharge (cfs)') %>%
      dySeries('discharge', strokeWidth = 3, color = 'black') %>%
      dySeries('baseflow', strokeWidth = 2, color = 'red', strokePattern = 'dashed') %>%
      dySeries('quickflow', strokeWidth = 2, color = 'forestgreen', strokePattern = 'dashed') %>%
      dySeries('precip', strokeWidth = 2, color = 'blue', axis = 'y2') %>%
      dyAxis("y2", label = "Precipitation (mm)", valueRange = c(max(plot_site$precip)*1.75, 0), axisLabelColor = 'blue') %>%
      dyRangeSelector() %>%
      dyLegend(labelsSeparateLines = TRUE, labelsDiv = 'div_pred_legend', show = 'always')
  })
  output$pred_legend <- renderUI({
    htmlOutput("div_pred_legend", height = "400px")
  })
  
  #####
  #---------------------------------------------------------------#

}