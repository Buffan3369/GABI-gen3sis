################################################################################
# Name: Get_clim_coord_Americas.R
# Author: Lucas Buffan (lucas.l.buffan@gmail.com)
# Aim: Extract xy coordinates for 1x1° climate grid used in our simulations (to
#      further upscale Oscillayers data).
################################################################################

mask_americas <- shapefile("./Data/Shapefile_masks/raw_mask.shp")
ref <- read.table("./Data/PALEO_PGEM-bioclim/bio1_mean.txt", header = T)
ref <- ref[, 1:3]

r_ref <- rasterFromXYZ(ref, crs = crs(mask_americas))
Am_extracted <- raster::extract(r_ref, mask_americas, cellnumbers = T, df = T)
xy_extracted <- coordinates(r_ref)[Am_extracted$cell,] %>% as.data.frame()

saveRDS(xy_extracted, "./Data/PALEO_PGEM-bioclim/Cell_Coordinates_Americas.RDS")