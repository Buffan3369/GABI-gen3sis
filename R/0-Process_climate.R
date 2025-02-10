################################################################################
# Name: 0-Process_climate.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Process MAT and MAP rasters before using them to desing gen3sis inputs.
#   WARNING => CANNOT BE RUN ON RSTUDIO
################################################################################

library(raster)
library(future.apply) # for parallel processing

## Function that will crop each climate raster based on the mask ---------------
process_raster <- function(time, mask, climate_dataset){
  # Extract corresponding raster and convert to stars object
  r <- rasterFromXYZ(climate_dataset[,c(1,2,time)])
  crs(r) <- crs(mask)
  # Extract values contained in the vector mask
  clim_am <- extract(r, mask, cellnumbers = TRUE, df = TRUE)
  # Retrieve coordinates of the corresponding cells
  xy_extracted <- coordinates(r)[clim_am$cell,]
  # Construct xyz file and create a raster out of it
  xyz_extracted <- data.frame(cbind(xy_extracted, clim_am[,3]))
  raster_extracted <- rasterFromXYZ(xyz = xyz_extracted, crs = crs(mask))
  return(raster_extracted)
}


## Set up multithreading -------------------------------------------------------
plan(multicore, workers = 20)

## Vector mask only encompassing continental Americas --------------------------
americas <- shapefile("./Data/Shapefile_masks/raw_mask.shp")

## Temperature portion of our landscape ----------------------------------------
# Open temperature file
MAT <- read.table("./Data/PALEO_PGEM-bioclim/bio1_mean.txt", header = T)
# Apply our raster processing function
temp_list <- future_lapply(X = 3:ncol(MAT), FUN = process_raster, mask = americas, climate_dataset = MAT)
#temp_list <- lapply(X = 3:18, FUN = process_raster, mask = americas, climate_dataset = MAT)
names(temp_list) <- as.character(seq(5000,1,-1))
saveRDS(object = temp_list, file = "./Data/Gen3sis_landscapes/MAT_list_Americas.RDS")
rm(MAT)

## Precipitations portion of our landscape -------------------------------------
# Open precipitation file
MAP <- read.table("./Data/PALEO_PGEM-bioclim/bio12_mean.txt", header = T)
# Apply our raster processing function
prec_list <- future_lapply(X = 3:ncol(MAP), FUN = process_raster, mask = americas, climate_dataset = MAP)
names(prec_list) <- as.character(seq(5000,1,-1))
saveRDS(object = prec_list, file = "./Data/Gen3sis_landscapes/MAP_list_Americas.RDS")
rm(MAP)

## Stop parallel workers -------------------------------------------------------
plan(sequential)


## Present-day topography ------------------------------------------------------
# TO UPDATE : MIND RESOLUTION (to be downscaled from c(1/30, 1/30) to c(1, 1))
world_elevation <- raster("./Data/World_elevation/NE2_50M_SR.tif")

mask <- shapefile("./Data/Shapefile_masks/raw_mask.shp")

cropped <- raster::extract(world_elevation, mask, cellnumbers = TRUE, df = TRUE)
xy_extracted <- coordinates(world_elevation)[cropped$cell,]
# Construct xyz file and create a raster out of it
xyz_extracted <- data.frame(cbind(xy_extracted, cropped[,3]))
raster_extracted <- rasterFromXYZ(xyz = xyz_extracted, crs = crs(mask))
plot(raster_extracted)

