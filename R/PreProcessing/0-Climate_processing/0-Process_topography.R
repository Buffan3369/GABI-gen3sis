################################################################################
# Name: 0-Process_topography.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Process world elevation topographic map (downloaded from naturalearthdata.com)
################################################################################

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

