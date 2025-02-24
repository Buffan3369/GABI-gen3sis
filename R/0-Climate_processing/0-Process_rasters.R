################################################################################
# Name: 0-Process_rasters.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Function to process rasters (extract values within a given area).
################################################################################

library(raster)

## Function that will crop each climate raster based on the mask ---------------
process_raster <- function(time, mask, climate_dataset, as_terra = FALSE){
  # Extract corresponding raster and convert to stars object
  r <- rasterFromXYZ(climate_dataset[,c(1,2,time)], crs = crs(mask))
  # Extract values contained in the vector mask
  clim_am <- extract(r, mask, cellnumbers = TRUE, df = TRUE)
  # Retrieve coordinates of the corresponding cells
  xy_extracted <- coordinates(r)[clim_am$cell,]
  # Construct xyz file and create a raster out of it
  xyz_extracted <- data.frame(cbind(xy_extracted, clim_am[,3]))
  raster_extracted <- rasterFromXYZ(xyz = xyz_extracted, crs = crs(mask))
  if(as_terra){
    # Convert to SpatRaster object
    raster_extracted <- as(raster_extracted, Class = "SpatRaster")
  }
  return(raster_extracted)
}

