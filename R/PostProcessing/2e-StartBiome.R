################################################################################
# Name: 5e-StartBiome.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Get the biome starting species belong to
################################################################################

library(raster)

## Open kg_biome map for t0 (5 Myr ago) ----------------------------------------
biome_5M <- readRDS("../Data/pClimate/KG_biomes/KG_5Ma_broadclass.RDS")
biome_5M <- biome_5M[[1]]

## Assess the biome to which the starting species of run i belongs -------------
get_biome_start <- function(i, start, model, eq_dist = FALSE){
  # Open initial presence-absence matrix
  if(eq_dist){
    init_mat <- readRDS(paste0("../Outputs_Eq_Dist/", model, "/", start, "_America_start/Config_", i, "/config_", model, 
                         "_", start, "_America_run_", i, "/pa_matrices/pa_t_499.rds"))
  }
  else{
    init_mat <- readRDS(paste0("../Outputs/", model, "/", start, "_America_start/config_", model,
                               "_", start, "_America_run_", i, "/pa_matrices/pa_t_499.rds"))
  }
  # Check whether starting species existed
  if(length(which(init_mat[,3] == 1)) > 0){
    
    ## Sample one of the starting cells ##
    spld <- sample(x = which(init_mat[,3] == 1), size = 1)
    start_xy <- init_mat[spld, c(1,2)]
    start_xy_df <- data.frame(x = start_xy[1], y = start_xy[2])
    
    ## Extract corresponding biome ##
    biome_start <- extract(biome_5M, start_xy_df)

    return(biome_start)
  }
  else{
    return(NA)
  }
}
