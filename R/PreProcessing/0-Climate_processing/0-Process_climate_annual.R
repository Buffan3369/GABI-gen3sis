################################################################################
# Name: 0-Process_climate_annual.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Process MAT and MAP rasters (annual averages) before using them to design
#   gen3sis inputs.
#   WARNING => CANNOT BE RUN IN RSTUDIO
################################################################################

library(future.apply) # for parallel processing
source("./0-Process_rasters.R")

## Set up multithreading -------------------------------------------------------
plan(multicore, workers = 35)

## Vector mask only encompassing continental Americas --------------------------
americas <- shapefile("../Data/Shapefile_masks/raw_mask.shp")


############################## ANNUAL AVERAGES #################################
## Temperature portion of our landscape ----------------------------------------
# Open temperature file
MAT <- read.table("../Data/pClimate/bio1_mean.txt", header = T)
# Apply our raster processing function
temp_list <- future_lapply(X = 3:ncol(MAT), FUN = process_raster, mask = americas, climate_dataset = MAT)
names(temp_list) <- as.character(seq(5000,1,-1))
saveRDS(object = temp_list, file = "../Data/pClimate/MAT_list_Americas.RDS")
rm(MAT)

## Precipitations portion of our landscape -------------------------------------
# Open precipitation file
MAP <- read.table("../Data/pClimate/bio12_mean.txt", header = T)
# Apply our raster processing function
prec_list <- future_lapply(X = 3:ncol(MAP), FUN = process_raster, mask = americas, climate_dataset = MAP)
names(prec_list) <- as.character(seq(5000,1,-1))
saveRDS(object = prec_list, file = "../Data/pClimate/MAP_list_Americas.RDS")
rm(MAP)

## Stop parallel workers -------------------------------------------------------
plan(sequential)
