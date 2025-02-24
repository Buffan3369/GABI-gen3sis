################################################################################
# Name: 0bis-Run_KG_climate_subdiv.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Process monthly climate rasters to obtain biome maps.
#       DONNOT RUN IN RSTUDIO
################################################################################

# library(future.apply)

source("0bis-Process_climate_KG.R")

# plan(multicore, workers = 10)
# biome_maps <- future_lapply(X = seq(1,5000,10),
#                             FUN = reclass_rasters)

biome_maps <- lapply(X = seq(1,5000,10),
                     FUN = reclass_rasters)

if(!dir.exists("../../Data/PALEO_PGEM-bioclim/KG_biomes/")){
  dir.create("../../Data/PALEO_PGEM-bioclim/KG_biomes/")
}
saveRDS(biome_maps, "../../Data/PALEO_PGEM-bioclim/KG_biomes/KG_biome_maps.RDS")