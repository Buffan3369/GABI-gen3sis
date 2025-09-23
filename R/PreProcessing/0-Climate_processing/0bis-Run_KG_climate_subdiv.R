################################################################################
# Name: 0bis-Run_KG_climate_subdiv.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Process monthly climate rasters to obtain biome maps.
#       DONNOT RUN IN RSTUDIO
################################################################################

#library(future.apply)

source("0bis-Process_climate_KG.R")

# Broad classification for t=5Myrs
biome_5M <- reclass_rasters(t = 0, type = "broadclass")
saveRDS(biome_5M, "../../Data/PALEO_PGEM-bioclim/KG_biomes/KG_5Ma_broadclass.RDS")

# Original classes proposed by Köppen-Geiger
plan(multicore, workers = 10)
biome_maps <- future_lapply(X = 0:5000,
                            FUN = reclass_rasters,
                            future.seed = TRUE) # avoids issue when `future` generates random numbers

if(!dir.exists("../../Data/PALEO_PGEM-bioclim/KG_biomes/")){
  dir.create("../../Data/PALEO_PGEM-bioclim/KG_biomes/")
}
saveRDS(biome_maps, "../../Data/PALEO_PGEM-bioclim/KG_biomes/KG_biome_maps.RDS")
