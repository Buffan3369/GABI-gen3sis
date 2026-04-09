################################################################################
# Name: 1-Process_climate_Oscillayers.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Process MAT and MAP rasters (annual averages) from Oscillayers 
#       before using them to design gen3sis landscape
#   WARNING => CANNOT BE RUN IN RSTUDIO
################################################################################

library(future.apply) # for parallel processing
source("./0-Upscale_rasters.R")

## Set up multithreading -------------------------------------------------------
plan(multicore, workers = 35)

xy_ref <- readRDS("../../Data/Cell_Coordinates_Americas.RDS")

## Temperature portion of our landscape ----------------------------------------
# Apply our raster processing function
# temp_list <- future_lapply(X = 2:500, FUN = process_raster, bioclim = 1, xy_ref = xy_ref, future.seed = T)
# names(temp_list) <- as.character(2:500)
# saveRDS(object = temp_list, file = "../../Data/pClimate/Oscillayers/MAT_list_Americas_Oscillayers.RDS")
# rm(temp_list)

## Precipitations portion of our landscape -------------------------------------
# Apply our raster processing function
prec_list <- future_lapply(X = 2:500, FUN = process_raster, bioclim = 12, xy_ref = xy_ref, future.seed = T)
names(prec_list) <- as.character(2:500)
saveRDS(object = prec_list, file = "../../Data/pClimate/Oscillayers/MAP_list_Americas_Oscillayers.RDS")
rm(prec_list)

## Stop parallel workers -------------------------------------------------------
plan(sequential)
