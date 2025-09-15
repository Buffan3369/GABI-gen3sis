################################################################################
# Name: 0q-Average_clim_per_continent.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Average climate estimates across space for North and South America 
################################################################################

library(raster)
library(tidyverse)

## Masks -----------------------------------------------------------------------
NthAm <- shapefile("./Data/Shapefile_masks/North_America_cut.shp")
SthAm <- shapefile("./Data/Shapefile_masks/South_America_cut.shp")

## Subset temperature values contained in Northern and Southern hemispheres ----
MAT <- read.table("./Data/PALEO_PGEM-bioclim/bio1_mean.txt", header = T)
MAT_5M <- MAT %>% dplyr::select(Long, Lat, T_5000)
  # North American cell coordinates
r_MAT_5M <- rasterFromXYZ(MAT_5M)
r_MAT_5M_North <- raster::extract(r_MAT_5M, NthAm, cellnumbers = TRUE, df = TRUE)
NA_coord <- coordinates(r_MAT_5M)[r_MAT_5M_North$cell,] %>% as.data.frame()
colnames(NA_coord) <- c("Long", "Lat")
MAT_North <- MAT %>%
  group_by(Long) %>% 
  filter(Lat %in% NA_coord$Lat & Long %in% NA_coord$Long)
  # Average temperature across space
MAT_North_av <- apply(X = MAT_North[, c(3:ncol(MAT_North))],
                      FUN = mean,
                      MARGIN = 2,
                      na.rm = T)
  # Same for South America
r_MAT_5M_South <- raster::extract(r_MAT_5M, SthAm, cellnumbers = TRUE, df = TRUE)
SA_coord <- coordinates(r_MAT_5M)[r_MAT_5M_South$cell,] %>% as.data.frame()
colnames(SA_coord) <- c("Long", "Lat")
MAT_South <- MAT %>%
  group_by(Long) %>% 
  filter(Lat %in% SA_coord$Lat & Long %in% SA_coord$Long)
# Average temperature across space
MAT_South_av <- apply(X = MAT_South[, c(3:ncol(MAT_South))],
                      FUN = mean,
                      MARGIN = 2,
                      na.rm = T)
rm(MAT, MAT_North, MAT_South)

## Precipitations (in mm) ------------------------------------------------------
MAP <- read.table("./Data/PALEO_PGEM-bioclim/bio12_mean.txt", header = T)
MAP_5M <- MAP %>% dplyr::select(Long, Lat, T_5000)
# North American cell coordinates
r_MAP_5M <- rasterFromXYZ(MAP_5M)
r_MAP_5M_North <- raster::extract(r_MAP_5M, NthAm, cellnumbers = TRUE, df = TRUE)
NA_coord <- coordinates(r_MAP_5M)[r_MAP_5M_North$cell,] %>% as.data.frame()
colnames(NA_coord) <- c("Long", "Lat")
MAP_North <- MAP %>%
  group_by(Long) %>% 
  filter(Lat %in% NA_coord$Lat & Long %in% NA_coord$Long)
# Average temperature across space
MAP_North_av <- apply(X = MAP_North[, c(3:ncol(MAP_North))],
                      FUN = mean,
                      MARGIN = 2,
                      na.rm = T)
# Same for South America
r_MAP_5M_South <- raster::extract(r_MAP_5M, SthAm, cellnumbers = TRUE, df = TRUE)
SA_coord <- coordinates(r_MAP_5M)[r_MAP_5M_South$cell,] %>% as.data.frame()
colnames(SA_coord) <- c("Long", "Lat")
MAP_South <- MAP %>%
  group_by(Long) %>% 
  filter(Lat %in% SA_coord$Lat & Long %in% SA_coord$Long)
# Average temperature across space
MAP_South_av <- apply(X = MAP_South[, c(3:ncol(MAP_South))],
                      FUN = mean,
                      MARGIN = 2,
                      na.rm = T)
rm(MAP, MAP_North, MAP_South)

## Dataframe -------------------------------------------------------------------
av_clim <- data.frame(time = seq(5000, 0, -1),
                      continent = rep(c(rep("North", 5001), rep("South", 5001)), 2),
                      var = c(rep("Temperature", 10002), rep("Precipitations", 10002)),
                      clim_av = c(MAT_North_av, MAT_South_av, MAP_North_av, MAP_South_av))
saveRDS(av_clim, "./Data/PALEO_PGEM-bioclim/average_clim_both_continents.RDS")
