################################################################################
# Name: 4-Extract_distribs_with_moments.R
# Author: Lucas Buffan (lucas.l.buffan@gmail.com)
# Aim: Functions to get plot-ready datasets with distributions and moments 
#      (mean and 95%) of distance to isthmus, proportion of colonised area and 
#      diversity within the colonised area
################################################################################

library(tidyverse)
source("./R/useful/helper_functions.R")

assign_sumtbl_to_metric <- function(metric){
  if(metric == "dist_to_isthmus"){
    sumtbl <- readRDS("./Results/Exchanged_metrics/Dist_isthmus.RDS")
  }
  else if(metric == "prop_col_area"){
    sumtbl <- readRDS("./Results/Exchanged_metrics/Prop_colonised_area.RDS")
  }
  else if(metric == "div_col"){
    sumtbl <- readRDS("./Results/Exchanged_metrics/Div_col_area.RDS")
  }
  else{
    stop(paste0("Metric not found: ", metric, "\nPlease check parameter table's column names."))
  }
}

summarise_distrib <- function(metric = c("dist_to_isthmus", "prop_col_area", "div_col"), # Metric we want to summarise
                              what = c("all", "no_crash", "exchanged)"), # Shall we keep all the runs, only those that didn't crash or those that resulted in a success
                              Log_transform = F){ # shall we log-transform the data (only relevant for diversity in colonised area) 
                                # Initialise dataframe
                                param_tbl <- data.frame(metric = NA,
                                                        model = NA,
                                                        start = NA,
                                                        mean = NA,
                                                        lower_ci = NA,
                                                        upper_ci = NA)
                                colnames(param_tbl)[1] <- metric
                                # Loop across models and starting regions
                                for(mdl in c("M0", "M1", "M2", "M3")){
                                  for(start in c("North","South")){
                                    # Get metric distribution along the runs
                                    p_tbl <- read.table(paste0("./Data/Gen3sis_parameter_tables/", mdl, "/", start,
                                                               "_America_parameters_EXTENDED_EXCH_AREA_DIV_DIST_biome.txt"),
                                                        sep = "\t", header = TRUE)
                                    
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
                                      select(metric, model, start)
                                    
                                    if(Log_transform){
                                      p_tbl[,1] <- log(p_tbl[,1], base = 10)
                                      # /!\ Log(mean(X)) != mean(Log(X))  /!\ 
                                      mean_log <- mean(p_tbl[,1], na.rm = T)
                                      p_tbl$mean <- mean_log
                                      
                                      sd_log <- sd(p_tbl[,1], na.rm = T)
                                      Lower_ci <- Student_CI(x_bar = mean_log,
                                                             n = nrow(p_tbl),
                                                             sigma = sd_log,
                                                             alpha = 0.05,
                                                             what = "Lower")
                                      p_tbl$lower_ci <- Lower_ci
                                      
                                      Upper_ci <- Student_CI(x_bar = mean_log,
                                                             n = nrow(p_tbl),
                                                             sigma = sd_log,
                                                             alpha = 0.05,
                                                             what = "Upper")
                                      p_tbl$upper_ci <- Upper_ci
                                    }
                                    
                                    else{
                                      # Add mean and CI from table
                                      sumtbl <- assign_sumtbl_to_metric(metric = metric)
                                      
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
                              
