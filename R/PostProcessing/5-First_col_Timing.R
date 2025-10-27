################################################################################
# Name: 5-First_col_Timing.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Assess the timing of the first colonisation for each successful run.
#       Meant to be run on bm10.
################################################################################

library(raster)
library(tidyverse)

args <- commandArgs(trailingOnly=TRUE)

model <- args[1] # model
start <- args[2] # starting landmass

## Open corresponding param table to retrive the indices of successful runs
param_tbl <- read.table(paste0("../Data/param_tables/", model, "/", start, 
                               "_America_parameters_EXTENDED_EXCH_AREA_DIV_DIST_biome.txt"),
                        sep = "\t", header = TRUE)
succ_runs <- as.numeric(rownames(param_tbl)[which(param_tbl$exchanged == 1)])

t_exch <- rep(-1, nrow(param_tbl))

## Open corresponding shapefile mask
if(start == "North"){
    mask <- shapefile("../Data/Shapefile_masks/South_America_cut.shp")
}
if(start == "South"){
    mask <- shapefile("../Data/Shapefile_masks/North_America_cut.shp")
}

## Loop across successful runs
for(run in succ_runs){
  dir_pa <- paste0("../Outputs/", model, "/", start, "_America_start/config_",
                   model, "_", start, "_America_run_", run, "/pa_matrices/")
  PA_matrices <- list.files(dir_pa)
  # Flag matrix names to order them forward-in-time
  for(i in 0:499){
    idx <- which(PA_matrices == paste0("pa_t_", i, ".rds"))
    if(500-i < 10){
      PA_matrices[idx] <- paste0("00", (500-i), "-", PA_matrices[idx])
    }
    else if(500-i < 100){
      PA_matrices[idx] <- paste0("0", (500-i), "-", PA_matrices[idx])
    }
    else{
      PA_matrices[idx] <- paste0((500-i), "-", PA_matrices[idx])
    }
  }
  PA_matrices <- PA_matrices[order(PA_matrices)]
  
  # Coordinates of the cells of any PA matrix falling within the mask
  pa_499_name <- strsplit(PA_matrices[1], split = "-")[[1]][2]
  pa_499 <- readRDS(paste0(dir_pa, pa_499_name))
  r_last <- rasterFromXYZ(pa_499, crs = crs(mask))
  extracted <- raster::extract(r_last, mask, df = TRUE, cellnumbers = TRUE)
  xy_mask <- xyFromCell(r_last, extracted$cell)
  
  # Go across PA matrices for all timesteps to record timing of the first colonisation
  i <- 1 # we don't care about the first time step, no colonisation possible
  exch <- F
  while(exch == F){
    i <- i + 1
    t <- 500 - i
    PA_t_name <- strsplit(PA_matrices[i], split = "-")[[1]][2]
    PA_t <- readRDS(paste0(dir_pa, PA_t_name))
    # merge with coordinates of cells falling within the mask
    merged_final <- merge(PA_t, xy_mask, by = c("x", "y"))
    if(ncol(merged_final) > 3){ # if more than one species represented
      composite <- apply(X = merged_final[,3:ncol(merged_final)], FUN = sum, MARGIN = 1)
    }
    else{
      composite <- merged_final[,3]
    }
    # if we have at least one species in the target landmass
    if(length(unique(composite)) > 1){
      exch <- T
    }
  }
  t_exch[run] <- t
}


param_tbl$time_exch <- t_exch
saveRDS(param_tbl, paste0("../Data/param_tables/", model, "/", start, 
                          "_America_parameters_EXTENDED_EXCH_AREA_DIV_DIST_biome_time.RDS"))
