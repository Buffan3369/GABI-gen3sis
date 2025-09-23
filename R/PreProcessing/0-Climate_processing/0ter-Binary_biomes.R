################################################################################
# Name: 0ter-Binary_biomes.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Reclassify KG biome maps into binary rasters.
#      nb. DOWNSCALED TO A 10kyr TIME STEP
################################################################################

library(tidyverse)
library(hash)

## Open Biome maps reclassified following Galván et al. (2023) -----------------
biome_maps <- readRDS("./Data/PALEO_PGEM-bioclim/KG_biomes/KG_biome_maps.RDS")
biome_maps_down <- biome_maps[seq(1, 5000, 10)] # temporal downscaling

# Reclassify into binary maps --------------------------------------------------
  # Biome broad classes 
broad_classes <- hash("tropical" = 1:3, 
                      "arid" = 4:7, 
                      "temperate" = 8:16,
                      "cold" = 17:28, 
                      "polar" = 29:30)
  # Reclassify function
broad_reclass_raster <- function(rstr, class){
  class_idx <- as.numeric(values(broad_classes[class]))
  rstr@data@values[which(rstr@data@values %in% class_idx)] <- 1
  rstr@data@values[which(rstr@data@values %in% class_idx == F & 
                           is.na(rstr@data@values) == F)] <- 0
  return(rstr)
}
  # Apply for each biome type except polar (not relevant in our case)
for(class in c("tropical", "arid", "temperate", "cold")){
  binary_biome <- lapply(X = biome_maps, FUN = broad_reclass_raster, class = class)
  saveRDS(binary_biome,
          paste0("./Data/PALEO_PGEM-bioclim/KG_biomes/binary_biomes/", class, "_binary.RDS"))
}
