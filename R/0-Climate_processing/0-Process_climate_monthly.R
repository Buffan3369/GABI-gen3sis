################################################################################
# Name: 0-Process_climate_monthly.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Process MAT and MAP rasters before using them to design gen3sis inputs.
#   WARNING => CANNOT BE RUN ON RSTUDIO
################################################################################

library(DDD)
library(future.apply) # for parallel processing
source("./0-Process_rasters.R")

## Set up multithreading -------------------------------------------------------
plan(multicore, workers = 40)

## Vector mask only encompassing continental Americas --------------------------
americas <- shapefile("../Data/Shapefile_masks/raw_mask.shp")

############################# MONTHLY AVERAGES #################################
args <- commandArgs(trailingOnly=TRUE)
i <- as.numeric(args[1]) # index of the month

month <- i
if(month < 10){
  month <- paste0("0", month)
}
## Monthly temperatures ------------------------------------------------------
Monthly_temp <- read.table(paste0("../Data/pClimate/monthly_mean/monthly_temperature", month, "M.txt"))
temp_list <- future_lapply(X = 3:ncol(Monthly_temp), 
                            FUN = process_raster, mask = americas, climate_dataset = Monthly_temp, as_terra = TRUE)
names(temp_list) <- as.character(seq(5000,1,-1))
saveRDS(object = temp_list, file = paste0("../Data/pClimate/monthly_mean_rasters/mean_temperature_list_", 
                                          month, "M_Americas.RDS"))
## Monthly precipitations ----------------------------------------------------
Monthly_prec <- read.table(paste0("../Data/pClimate/monthly_mean/monthly_precipitation", month, "M.txt"))
prec_list <- future_lapply(X = 3:ncol(Monthly_prec), 
                           FUN = process_raster, mask = americas, climate_dataset = Monthly_prec, as_terra = TRUE)
names(prec_list) <- as.character(seq(5000,1,-1))
saveRDS(object = prec_list, file = paste0("../Data/pClimate/monthly_mean_rasters/mean_precipitation_list_", 
                                          month, "M_Americas.RDS"))

## Stop parallel workers -------------------------------------------------------
plan(sequential)
