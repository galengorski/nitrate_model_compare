#install.packages("hydroGOF")
library("hydroGOF")
#install.packages("Metrics")
library("Metrics")
#install.packages("infotheo")
library("infotheo")
#install.packages("tidyverse")
library("tidyverse")
#install.packages("magrittr")
library("magrittr")




#functions for custom metrics
MI <- function(x,y){
  temp <- cbind(x,y)
  
  dat <- discretize(temp, disc = 'equalwidth', nbins = 11)
  h_obs <- entropy(dat[,1], method = "emp") %>%
    natstobits()
  
  h_pred <- entropy(dat[,2], method = "emp") %>%
    natstobits
  
  mutualinformation <- multiinformation(dat, method = 'emp') %>% 
    natstobits() %>%
    divide_by(max(h_obs,h_pred)) %>%
    round(.,3)
  return(mutualinformation)
}

nrmse_func <-  function(obs, pred, type = "sd") {
  
  squared_sums <- sum((obs - pred)^2)
  mse <- squared_sums/length(obs)
  rmse <- sqrt(mse)
  if (type == "sd") nrmse <- rmse/sd(obs)
  if (type == "mean") nrmse <- rmse/mean(obs)
  if (type == "maxmin") nrmse <- rmse/ (max(obs) - min(obs))
  if (type == "iq") nrmse <- rmse/ (quantile(obs, 0.75) - quantile(obs, 0.25))
  if (!type %in% c("mean", "sd", "maxmin", "iq")) message("Wrong type!")
  nrmse <- round(nrmse, 3)
  return(nrmse)
  
}

#script for grouping modeling results into a single list

sites <- grep(list.files('03_indv_models_32/model_output_nitrate'), pattern='Icon', invert=TRUE, value=TRUE)
model_metrics <- data.frame()

for(i in 1:length(sites)){
#read in predictors
pred <- read.csv(paste0('06_perform_baseflow_separation/output_data/',sites[i],'_attr_bfs.csv'))

#read in local models
local <- read.csv(paste0('03_indv_models_32/model_output_nitrate/',sites[i],'/ModelResults.csv'))
#local model with baseflow separation
local_bfs <- read.csv(paste0('03_indv_models_32/model_output_nitrate_bfs/',sites[i],'/ModelResults.csv'))

#read in all watershed models
all_watershed <- read.csv(paste0('04_reg_models_32/model_output_nitrate/N01/',sites[i],'/ModelResults.csv'))

#read in all watershed models with baseflow separation
all_watershed_bfs <- read.csv(paste0('04_reg_models_32/model_output_nitrate/N02/',sites[i],'/ModelResults.csv'))

#read in all watershed models with baseflow separation and watershed attributes
all_watershed_bfs_attr <- read.csv(paste0('04_reg_models_32/model_output_nitrate/N03/',sites[i],'/ModelResults.csv'))

#read in all watershed models with watershed attributes
all_watershed_attr <- read.csv(paste0('04_reg_models_32/model_output_nitrate/N04/',sites[i],'/ModelResults.csv'))

#merge predictors with local data so that the predictor can be selected
pred_local <- merge(pred, local, by.x = 'Date', by.y = 'DateTime', all.x = FALSE, all.y = TRUE)
model_results = data.frame(DateTime = local$DateTime, CalibValid = local$Calib.Valid, 
                           discharge = pred_local$discharge, quickflow = pred_local$qft , baseflow = pred_local$bt,
                           temp_min = pred_local$tmin,temp_max = pred_local$tmax, precip = pred_local$prcp, 
                           Observed = local$Labeled,
                           local = local$Predicted, local_bfs = local_bfs$Predicted, all_ws = all_watershed$Predicted,
                           all_ws_attr = all_watershed_attr$Predicted, all_ws_bfs = all_watershed_bfs$Predicted,
                           all_ws_bfs_attr = all_watershed_bfs_attr$Predicted)
model_results_valid <- model_results[model_results$CalibValid == 'Valid',]
model_results_calib <- model_results[model_results$CalibValid == 'Calib',]




#make model metrics table
calib_metrics <- t(rbind(sapply(model_results_calib[,c(10:15)],  Metrics::rmse, model_results_calib$Observed),
                         sapply(model_results_calib[,c(10:15)],  nrmse_func, obs = model_results_calib$Observed, type = 'mean'),
                         sapply(model_results_calib[,c(10:15)],  nrmse_func, obs = model_results_calib$Observed, type = 'iq'),
                         sapply(model_results_calib[,c(10:15)],  MI, model_results_calib$Observed)))
colnames(calib_metrics) <- c('RMSE_Calib','NRMSE_Calib_mean','NRMSE_Calib_iq','MI_Calib')
valid_metrics <- t(rbind(sapply(model_results_valid[,c(10:15)],  Metrics::rmse, model_results_valid$Observed),
                         sapply(model_results_valid[,c(10:15)],  nrmse_func, obs = model_results_valid$Observed, type = 'mean'),
                         sapply(model_results_valid[,c(10:15)],  nrmse_func, obs = model_results_valid$Observed, type = 'iq'),
                         sapply(model_results_valid[,c(10:15)],  MI, model_results_valid$Observed)))
colnames(valid_metrics) <- c('RMSE_Valid','NRMSE_Valid_mean','NRMSE_Valid_iq','MI_Valid')
merged <- merge(calib_metrics, valid_metrics, by = 0)
colnames(merged)[1] = 'Model'
merged$Site <- sites[i]
model_metrics <- rbind(model_metrics, merged)

write.csv(model_results_valid, paste0('07_comparing_nitrate_models/output_data/', sites[i],'.csv', sep = ''), row.names = F)
}

write.csv(model_metrics, '07_comparing_nitrate_models/output_data/model_metrics_new.csv', row.names = FALSE)