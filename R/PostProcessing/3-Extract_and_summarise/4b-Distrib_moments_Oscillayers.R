################################################################################
# Name: 4b-Distrib_moments_Oscillayers.R
# Author: Lucas Buffan (lucas.l.buffan@gmail.com)
# Aim: Functions to get plot-ready datasets with distributions and moments 
#      (mean and 95%) of distance to isthmus, proportion of colonised area and 
#      diversity within the colonised area for Oscillayers-based simulations
################################################################################

library(tidyverse)
source("./R/useful/helper_functions.R")

assign_sumtbl_to_metric <- function(metric){
  if(metric == "dist_to_isthmus"){
    sumtbl <- readRDS(paste0("./Results/Exchanged_metrics/Oscillayers/Dist_isthmus.RDS"))
  }
  else if(metric == "prop_col_area"){
    sumtbl <- readRDS(paste0("./Results/Exchanged_metrics/Oscillayers/Prop_colonised_area.RDS"))
  }
  else if(metric == "abs_col_area"){
    sumtbl <- readRDS(paste0("./Results/Exchanged_metrics/Oscillayers/Absolute_colonised_area.RDS"))
  }
  else if(metric == "div_col"){
    sumtbl <- readRDS(paste0("./Results/Exchanged_metrics/Oscillayers/Div_col_area.RDS"))
  }
  else{
    stop(paste0("Metric not found: ", metric, "\nPlease check parameter table's column names."))
  }
}

summarise_distrib <- function(metric = c("dist_to_isthmus", "prop_col_area", "abs_col_area",
                                         "div_col")){ # Metric we want to summarise
  # Initialise dataframe
  param_tbl <- data.frame(metric = NA,
                          model = NA,
                          start = NA,
                          mean = NA,
                          lower_ci = NA,
                          upper_ci = NA)
  colnames(param_tbl)[1] <- metric
  # Loop across models and starting regions
  for(mdl in c("M0", "M1")){
    for(start in c("North","South")){
      # Get metric distribution along the runs
      p_tbl <- read.table(paste0("./Data/Gen3sis_parameter_tables/Oscillayers/", mdl, "/", start,
                              "_America_parameters_EXTENDED_EXCH_AREA_DIV_DIST.txt"), header = T)
      
      # Filter simulations leading to successful exchange
      p_tbl <- p_tbl %>% filter(exchanged == 1)

      p_tbl$model <- mdl
      p_tbl$start <- start
      p_tbl <- p_tbl %>% 
        dplyr::select(metric, model, start)
      
      # Add mean and CI from table
      sumtbl <- assign_sumtbl_to_metric(metric = metric)
      
      p_tbl$mean <- sumtbl[which(sumtbl$Ori == paste0(start, " America") & 
                                   sumtbl$Model == mdl),2] # second column is for the mean estimate, unstandardised name
      p_tbl$lower_ci <- sumtbl$Lower_CI[which(sumtbl$Ori == paste0(start, " America") & 
                                                sumtbl$Model == mdl)]
      p_tbl$upper_ci <- sumtbl$Upper_CI[which(sumtbl$Ori == paste0(start, " America") & 
                                                sumtbl$Model == mdl)]
      # Concatenate dfs
      param_tbl <- rbind.data.frame(param_tbl, p_tbl)
    }
  }
  param_tbl <- param_tbl[2:nrow(param_tbl),]
  rownames(param_tbl) <- 1:nrow(param_tbl)
  return(param_tbl)
} 

