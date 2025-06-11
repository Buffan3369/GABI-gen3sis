################################################################################
# Name: 1b-Generate_binary_biome_landscapes.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Genrate landscape from binary biome rasters (downscaled to a 10kyr step)
# /!\ Run on BIGMEM /!\
################################################################################

library(gen3sis)

args <- commandArgs(trailingOnly=TRUE)

biome <- as.character(args[1]) # within c("tropical", "arid", "temperate", "cold")

## Open environment layers -----------------------------------------------------
biome_rast_list <- readRDS(paste0("../Data/pClimate/KG_biomes/binary_biomes/", biome, "_binary.RDS"))
## Generate Landscape ----------------------------------------------------------
  # Cost function
cost_function <- function(source, habitable_src, dest, habitable_dest) {
  if(!all(habitable_src, habitable_dest)) {
    return(2/1000)
  } else {
    return(1/1000)
  }
}
  # Landscape per-se
landscape_list <- list(biome = biome_rast_list)
create_input_landscape(landscapes = landscape_list,
                       timesteps = as.character(1:500),
                       cost_function = cost_function,
                       directions = 8,
                       output_directory = paste0("../Data/Landscape/binary_biomes/", biome),
                       overwrite_output = T,
                       crs = "+proj=longlat +datum=WGS84 +no_defs",
                       calculate_full_distance_matrices = T)

