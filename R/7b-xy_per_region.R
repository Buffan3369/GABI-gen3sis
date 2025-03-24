################################################################################
# Name: 7b-xy_per_region.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Extract coordinates of PA matrix cells falling either in North or South
#       America.
################################################################################

library(raster)

## Masks ----------------------------------------------------------------------- 
SAm <- shapefile("./Data/Shapefile_masks/South_America_cut.shp")
NAm <- shapefile("./Data/Shapefile_masks/North_America_cut.shp")

## Retrieve coordinates of the cells falling in South and North America --------
pa_first <- readRDS("./Results/M1_eq_area/South_America_start/config_M1_South_America_run_265/pa_matrices/pa_t_499.rds")
r_first <- rasterFromXYZ(pa_first)
# North America
extracted_North <- extract(r_first, NAm, df = TRUE, cellnumbers = TRUE)
xy_NA <- xyFromCell(r_first, extracted_North$cell)
saveRDS(xy_NA, "./Data/pa_cells_North_South/xy_North_Am.RDS")
# South America
extracted_South <- extract(r_first, SAm, df = TRUE, cellnumbers = TRUE)
xy_SA <- xyFromCell(r_first, extracted_South$cell)
saveRDS(xy_SA, "./Data/pa_cells_North_South/xy_South_Am.RDS")
