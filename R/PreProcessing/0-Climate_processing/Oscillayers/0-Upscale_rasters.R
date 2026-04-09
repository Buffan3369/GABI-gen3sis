################################################################################
# Name: 0-Process_rasters.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Function to process rasters (extract climate values within the Americas)
#       /!\ MEANT TO BE RUN ON BIGMEM /!\
################################################################################

library(raster)

## Function that will crop and downscale rasters from Oscillayers --------------
process_raster <- function(time, # in kyr ago
                           bioclim, # 1 or 12
                           xy_ref){
  path <- paste0("../../Data/pClimate/Oscillayers/bio", bioclim, "/bio",
                 bioclim, "_t", time, ".asc")
  # Corresponding raster
  r_clim <- raster(path)
  # Extract values contained in the vector mask
  clim_am <- raster::extract(r_clim, xy_ref, cellnumbers = TRUE, df = TRUE)
  # Construct xyz file and create a raster out of it
  xyz_extracted <- data.frame(cbind(xy_ref, clim_am[,3]))
  raster_extracted <- rasterFromXYZ(xyz = xyz_extracted, crs = crs(r_clim))
  return(raster_extracted)
}

