library(tidyverse)
library(raster)


# Mask of the Americas
Americas <- shapefile("./Data/Shapefile_masks/raw_mask.shp")
# Temperature
Temp <- read.table("./Data/PALEO_PGEM-bioclim/bio1_mean.txt", header = T)
Temp_sub <- Temp %>% dplyr::select(Long, Lat, T_5000, T_3000, T_2580, T_1000, T_21, T_12, T_0)
rm(Temp)
# Precipitations
Prec <- read.table("./Data/PALEO_PGEM-bioclim/bio12_mean.txt", header = T)
Prec_sub <- Prec %>% dplyr::select(Long, Lat, T_5000, T_3000, T_2580, T_1000, T_21, T_12, T_0)
rm(Prec)
# KG biomes
biomes <- readRDS("./Data/PALEO_PGEM-bioclim/KG_biomes/KG_biome_maps.RDS")

for(i in c(5000, 3000, 2580, 21, 12, 0)){
  # Subset temperature estimates for the Americas
  tmp_Temp <- Temp_sub[, c("Long", "Lat", paste0("T_", i))]
  r_Temp <- rasterFromXYZ(tmp_Temp)
  r_Temp_Am <- raster::extract(r_Temp, Americas, cellnumbers = TRUE, df = TRUE)
  xy_extracted <- coordinates(r_Temp)[r_Temp_Am$cell,] %>% as.data.frame()
  xyz_extracted <- data.frame(cbind(xy_extracted, r_Temp_Am[,3]))
  r_extracted_Temp <- rasterFromXYZ(xyz_extracted, crs = crs(Americas))
  # Same for precipitations
  tmp_Prec <- Prec_sub[, c("Long", "Lat", paste0("T_", i))]
  r_Prec <- rasterFromXYZ(tmp_Prec)
  r_Prec_Am <- raster::extract(r_Prec, Americas, cellnumbers = TRUE, df = TRUE)
  xy_extracted <- coordinates(r_Prec)[r_Prec_Am$cell,] %>% as.data.frame()
  xyz_extracted <- data.frame(cbind(xy_extracted, r_Prec_Am[,3]))
  r_extracted_Prec <- rasterFromXYZ(xyz_extracted, crs = crs(Americas))
  # Subset corresponding biome
  corr_biome <- biomes[[i]] # Need to switch to broad classes
}
