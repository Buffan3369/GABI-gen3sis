################################################################################
# Name: 10-Starting_biome.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Get the biome starting species belong to
################################################################################

library(raster)

## Open kg_biome map for t0 (5 Myr ago) ----------------------------------------
biome_5M <- readRDS("../Data/pClimate/KG_biomes/KG_5Ma_broadclass.RDS")
biome_5M <- biome_5M[[1]]

## Assess the biome to which the starting species of run i belongs -------------
  get_biome_start <- function(i, start, model){
    init_mat <- readRDS(paste0("../Outputs/", model, "/", start, "_America_start/", 
                               "config_", model, "_", start, "_America_run_", i, "/pa_matrices/pa_t_499.rds"))
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

## Apply -----------------------------------------------------------------------
for(model in c("M0", "M1", "M2", "M3")){
  for(start in c("North", "South")){
    # Assess distance between simulation starting point and isthmus
    biomes <- sapply(X = 1:500, FUN = get_biome_start, start = start, model = model)
    # Open param table and save it
    param_tbl <- read.table(paste0("../Data/param_tables/", model, "/", start, "_America_parameters_EXTENDED_EXCH_AREA_DIV_DIST.txt"),
                            header = T)
    param_tbl$start_biome <- biomes
    # Save
    write.table(param_tbl, paste0("../Data/param_tables/", model, "/", start, "_America_parameters_EXTENDED_EXCH_AREA_DIV_DIST_biome.txt"))
  }
}