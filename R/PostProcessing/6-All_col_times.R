################################################################################
# Name: 6-All_col_times.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Assess the timing of each successful colonisation for each scenario and 
#       ancestral landmass.
################################################################################

library(raster)
library(tidyverse)

## Design function to retrieve distrib of col times ----------------------------
col_time_distrib <- function(run, model, start){
  # Open corresponding shapefile mask
  if(start == "North"){
    mask <- shapefile("../Data/Shapefile_masks/South_America_cut.shp")
  }
  if(start == "South"){
    mask <- shapefile("../Data/Shapefile_masks/North_America_cut.shp")
  }
  
  # Get the final time step of the simulation
  dir <- paste0("../Outputs/", model, "/", start, "_America_start/config_",
                model, "_", start, "_America_run_", run)
  LF <- list.files(paste0(dir, "/pa_matrices"))
  t_end <- 500-length(LF)
  
  # Coordinates of the cells of any PA matrix falling within the mask
  pa_end <- readRDS(paste0(dir, "/pa_matrices/pa_t_", t_end, ".rds"))
  r_last <- rasterFromXYZ(pa_end[,1:3], crs = crs(mask))
  extracted <- raster::extract(r_last, mask, df = TRUE, cellnumbers = TRUE)
  xy_mask <- xyFromCell(r_last, extracted$cell)
  
  # Retrieve origination times of each simulated species
  phy <- read.table(paste0(dir, "/phy.txt"),
                    sep = "\t", header = T)
  ori_times <- sort(unique(phy$Speciation.Time))
  ori_times <- ori_times[which(ori_times != t_end)] # those originating at t=0 (end) are not informative
  
  t_col <- c() # will store colonisation times
  sp_col <- c() # will store the corresponding coloniser species
  # Loop across each origination time
  for(t_ori in ori_times){
    # Get the species that originated at that time
    yaki_t_ori <- phy$Descendent[phy$Speciation.Time == t_ori]
    # Loop across those species to potentially identify colonisers
    for(sp in yaki_t_ori){
      pa_sp <- readRDS(paste0(dir, "/pa_per_sp/Species_", sp, ".rds"))
      # Add longitude and latitude
      pa_sp <- pa_sp %>% 
        add_column(x = pa_end[,1], y = pa_end[,2], .before = "t_0")
      # Outcrop region to colonise
      merged <- merge(pa_sp, xy_mask, by = c("x", "y"))
      # ---
      # If the species originated in this region, ignore it (we're looking for colonisers)
      # Otherwise, retrieve the time of colonisation (if it exists)
      # nb. We couldn't just eliminate species absent from the area to colonise at 
      #     t_end as it would ignore potential local extinctions
      # ---
      pa_sp_t_ori <- merged[, paste0("t_", t_ori)]
      if(1 %in% pa_sp_t_ori == F){
        t <- t_ori
        exch <- F
        while(exch == F){
          t <- t-1
          pa_sp_t <- merged[, paste0("t_", t)]
          if(1 %in% pa_sp_t){
            t_col <- c(t_col, t)
            sp_col <- c(sp_col, sp)
            exch <- T
          }
          if(t == t_end){
            break
          }
        }
      }
    }
  }
  
  if(dir.exists(paste0(dir, "/col_df/")) == F){
    dir.create(paste0(dir, "/col_df/"))
  }
  
  df_run <- data.frame(time = t_col,
                       species = sp_col)
  
  saveRDS(df_run, paste0(dir, "/col_df/col_df_run_", run, ".rds"))
}

## Execute ---------------------------------------------------------------------
  # Arguments
args <- commandArgs(trailingOnly=TRUE)
model <- args[1] # model
start <- args[2] # starting landmass
  # Successful runs
succ_runs <- c()
for(i in 1:500){
  dir <- paste0("../Outputs/", model, "/", start, "_America_start/config_",
                model, "_", start, "_America_run_", i)
  if(dir.exists(paste0(dir, "/pa_per_sp"))){
    succ_runs <- c(succ_runs, i)
  }
}
  # Proper execution across successful runs
for(run in succ_runs){
  col_time_distrib(run = run, model = model, start = start)
}


