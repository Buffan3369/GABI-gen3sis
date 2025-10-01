################################################################################
# Title: Harmonise_dists_to_isthm.R
# Author: Lucas Buffan (lucas.l.buffan@gmail.com)
# Aim: Compute and save distance of each cell of the 1x1° North America grid 
#      to the ithsmus of Panama.
################################################################################


library(raster)
library(spaths)
library(tidyverse)


## Arbitrary 1x1° map (the only thing that matters is the fact that we have a 1x1° map) --
clim_map <- readRDS("./Data/PALEO_PGEM-bioclim/KG_biomes/KG_5Ma_broadclass.RDS")

# North America mask
NthAm <- shapefile("./Data/Shapefile_masks/North_America_cut.shp")

# Subset North american cells
extracted <- extract(clim_map, NthAm, df = TRUE, cellnumbers = TRUE)
xy_north <- xyFromCell(clim_map, extracted$cell) %>% as.data.frame()

## Compute distance to isthmus (from "./R/PostProcessing/2d-DistIsthm.R") ------
assess_dist_isthm <- function(i){
  x <- xy_north$x[i]
  y <- xy_north$y[i]
  # Arbitrary point located near the Panamá isthmus
  isthm_coord <- c(-84, 10) 
  # Merge
  pts_df <- data.frame(rbind(c(x, y), isthm_coord))
  colnames(pts_df) <- c("lon", "lat")
  # Convert to spatial point
  start_sp <- SpatialPoints(pts_df[1,], proj4string = crs(clim_map))
  isthm_sp <- SpatialPoints(pts_df[2,], proj4string = crs(clim_map))
  # Compute shortest path between points on the rasterised landscape
  
  # Visual inspection
  # xy_tot <- as.data.frame(clim_map, xy = T)
  # colnames(xy_tot) <- c("lon", "lat", "biome")
  # ggplot() +
  #   geom_tile(data = xy_tot, aes(x = lon, y = lat, fill = biome)) +
  #   geom_point(data = pts_df, aes(x = lon, y = lat))
  
  if(is.na(extract(clim_map, start_sp))){
    return(NA)
  }
  else{
    dist <- spaths::shortest_paths(rst = clim_map, origins = start_sp, destinations = isthm_sp)
    return(dist$distances / 1e3) # in km
  }
}

## Apply & save table ----------------------------------------------------------
xy_north <- xy_north %>% mutate(dist = sapply(X = 1:nrow(xy_north),
                                              FUN = assess_dist_isthm))
saveRDS(xy_north, "./Data/Gen3sis_configs/North_American_cells_distances_to_isthmus.RDS")

