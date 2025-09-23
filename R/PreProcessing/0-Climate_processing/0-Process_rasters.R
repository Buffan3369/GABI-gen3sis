################################################################################
# Name: 0-Process_rasters.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Function to process rasters (extract values within a given area).
#       /!\ MEANT TO BE RUN ON BIGMEM /!\
################################################################################

library(raster)

## Function that will crop each climate raster based on the mask ---------------
process_raster <- function(time, # /!\ [3,4,...,5002] (what has to be given) <=> [5000,4999,...,1] (what it actually corresponds to) /!\
                           mask, climate_dataset, month,
                           what = c("temperature", "precipitation")){
  # Corresponding raster
  r <- rasterFromXYZ(climate_dataset[,c(1,2,time)], crs = crs(mask))
  # Extract values contained in the vector mask
  clim_am <- raster::extract(r, mask, cellnumbers = TRUE, df = TRUE)
  # Retrieve coordinates of the corresponding cells
  xy_extracted <- coordinates(r)[clim_am$cell,]
  # Construct xyz file and create a raster out of it
  xyz_extracted <- data.frame(cbind(xy_extracted, clim_am[,3]))
  raster_extracted <- rasterFromXYZ(xyz = xyz_extracted, crs = crs(mask))
  # Save it as an .RDS file in the corresponding directory
  if(month < 10){
    month <- paste0("0", month)
  }
  true_time <- 5001 - (time-2)
  out_dir <- paste0("../../Data/PALEO_PGEM-bioclim/monthly_mean_rasters/mean_",
                    what, "_", true_time, "k_", month, "M_Americas.RDS")
  saveRDS(raster_extracted, out_dir)
}

