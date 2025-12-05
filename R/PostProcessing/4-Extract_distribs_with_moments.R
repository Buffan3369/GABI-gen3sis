################################################################################
# Name: 4-Extract_distribs_with_moments.R
# Author: Lucas Buffan (lucas.l.buffan@gmail.com)
# Aim: Functions to get plot-ready datasets with distributions and moments 
#      (mean and 95%) of distance to isthmus, proportion of colonised area and 
#      diversity within the colonised area
################################################################################

library(tidyverse)
source("./R/useful/helper_functions.R")

assign_sumtbl_to_metric <- function(metric, 
                                    what=c("all", "no_crash", "exchanged"), # all only for dist_to_isthmus
                                    dist_std = F){ # if dist_std set to TRUE, treats outputs with North American distances to isthmus standardised with SA
  std <- ""
  all <- ""
  if(dist_std){
    std <- "_STD"
  }
  if(what == "all"){
    all <- "_ALL"
  }
  if(metric == "dist_to_isthmus"){
    sumtbl <- readRDS(paste0("./Results/Exchanged_metrics/Dist_isthmus", all, std, ".RDS"))
  }
  else if(metric == "prop_col_area"){
    sumtbl <- readRDS(paste0("./Results/Exchanged_metrics/Prop_colonised_area", all, std, ".RDS"))
  }
  else if(metric == "absolute_area"){
    sumtbl <- readRDS(paste0("./Results/Exchanged_metrics/Absolute_colonised_area.RDS", all, std, ".RDS"))
  }
  else if(metric == "div_col"){
    sumtbl <- readRDS(paste0("./Results/Exchanged_metrics/Div_col_area", all, std, ".RDS"))
  }
  else{
    stop(paste0("Metric not found: ", metric, "\nPlease check parameter table's column names."))
  }
}

summarise_distrib <- function(metric = c("dist_to_isthmus", "prop_col_area", "absolute_area",
                                         "div_col", "start_biome"), # Metric we want to summarise
                              what = c("all", "no_crash", "exchanged)"), # Shall we keep all the runs, only those that didn't crash or those that resulted in a success
                              dist_std = F){
                                # Initialise dataframe
                                if(metric == "start_biome"){
                                  param_tbl <- data.frame(metric = NA,
                                                          model = NA,
                                                          start = NA)
                                }
                                else{
                                  param_tbl <- data.frame(metric = NA,
                                                          model = NA,
                                                          start = NA,
                                                          mean = NA,
                                                          lower_ci = NA,
                                                          upper_ci = NA)
                                }
                                colnames(param_tbl)[1] <- metric
                                # Loop across models and starting regions
                                for(mdl in c("M0", "M1", "M2", "M3")){
                                  for(start in c("North","South")){
                                    # Get metric distribution along the runs
                                    p_tbl <- read.table(paste0("./Data/Gen3sis_parameter_tables/", mdl, "/", start,
                                                               "_America_parameters_EXTENDED_EXCH_AREA_DIV_DIST_biome.txt"),
                                                        sep = "\t", header = TRUE)
                                    
                                    if(dist_std & start == "North"){
                                      p_tbl <- read.table(paste0("./Data/Gen3sis_parameter_tables/Equal_dist/", mdl, "/", start,
                                                                 "_America_parameters_EqDist_EXTENDED_EXCH_AREA_DIV_DIST_biome.txt"),
                                                          sep = "\t", header = TRUE)
                                    }
                                    
                                    if(what == "no_crash"){
                                      p_tbl <- p_tbl %>% filter(exchanged != -1)
                                    }
                                    else if(what == "exchanged"){
                                      p_tbl <- p_tbl %>% filter(exchanged == 1)
                                    }
                                    else if(what %in% c("all", "no_crash", "exchanged)") == FALSE){
                                      stop(paste0("Argument `what` oddly provided."))
                                    }
                                    
                                    p_tbl$model <- mdl
                                    p_tbl$start <- start
                                    p_tbl <- p_tbl %>% 
                                      dplyr::select(metric, model, start)
                                    
                                    if(metric != "start_biome"){ # if start_biome, we don't want the mean and CI
                                      # Add mean and CI from table
                                      sumtbl <- assign_sumtbl_to_metric(metric = metric, what = what, dist_std = dist_std)
                                      
                                      p_tbl$mean <- sumtbl[which(sumtbl$Ori == paste0(start, " America") & 
                                                                   sumtbl$Model == mdl),2] # second column is for the mean estimate, unstandardised name
                                      p_tbl$lower_ci <- sumtbl$Lower_CI[which(sumtbl$Ori == paste0(start, " America") & 
                                                                                sumtbl$Model == mdl)]
                                      p_tbl$upper_ci <- sumtbl$Upper_CI[which(sumtbl$Ori == paste0(start, " America") & 
                                                                                sumtbl$Model == mdl)]
                                    }
                                    # Concatenate dfs
                                    param_tbl <- rbind.data.frame(param_tbl, p_tbl)
                                  }
                                }
                                param_tbl <- param_tbl[2:nrow(param_tbl),]
                                rownames(param_tbl) <- 1:nrow(param_tbl)
                                return(param_tbl)
                              } 
                              
