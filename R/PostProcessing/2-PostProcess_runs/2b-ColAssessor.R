################################################################################
# Name: 2b_ColAssessor.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: This function assesses whether a simulation led to successful interchange.
#       Meant to be run on bm10.
################################################################################

library(raster)
library(dplyr)
source("helper_functions.R")


## Define transfert function ---------------------------------------------------
# ----------------------------------------------------- #
# Returns 1 if run i resulted in a successful transfert #
# ----------------------------------------------------- #

transfert <- function(i,         # Run index
                      mdl,       # Scenario (M0, M1, ..)
                      n_sim,
                      from,      # Ancestral area ("North" or "South")
                      eq_dist,   # are we treating outputs from simulations generated after standardising initial distances to isthmus?
                      oscill,    # are we treating outputs from Oscillayers-based simulations?
                      expanded,  # are we treating outputs from simulations with thermal niche expanded?
                      w){
  E <- 0 # exchange metric
  # Define mask
  if(from == "North"){
    mask <- shapefile("../../Data/Shapefile_masks/South_America_cut.shp")
  }
  if(from == "South"){
    mask <- shapefile("../../Data/Shapefile_masks/North_America_cut.shp")
  }
  # Define directory
  suff <- ""
  if(eq_dist){
    suff <- "_Eq_Dist"
  }
  else if(oscill){
    if(expanded){
      suff <- paste0("_Oscillayers_expanded_w_", w)
    }
    else{
      suff <- "_Oscillayers"
    }
  }
  else if(oscill == FALSE & expanded){
    suff <- paste0("_expanded_w_", w)
  }
  dir <- paste0("../../Outputs", suff, "/", mdl, "/", from, "_America_start/Config_", i,
                "/config_", mdl, "_", from, "_America_run_", i, "/pa_matrices")
  
  if(dir.exists(dir)){
    # Open the presence/absence matrix of the last time step and ratserise it (1 ancestor => perfect xyz table)
    t_end <- n_sim - length(list.files(paste0(dir)))
    pa_last <- readRDS(paste0(dir, "/pa_t_", t_end, ".rds"))
    r_last <- rasterFromXYZ(pa_last[,1:3])
    # Extract coordinates of the cells falling within the mask
    extracted <- extract(r_last, mask, df = TRUE, cellnumbers = TRUE)
    xy <- xyFromCell(r_last, extracted$cell)
    # Merge
    merged_final <- merge(pa_last, xy, by = c("x", "y"))
    # Detect for exchange
    if(ncol(merged_final) > 3){
      colons <- apply(X = merged_final[,3:ncol(merged_final)],
                      FUN = ones,
                      MARGIN = 2)
    }
    if(ncol(merged_final) == 3){
      if(length(unique(pa_last[,3])) == 1){
          # if simulation aborpted 
          if(unique(pa_last[,3]) == 0){
            colons <- FALSE
            E <- -1
          }
          # if only one remaining species, we test if it crossed the isthmus or not
          else{
            colons <- ones(merged_final[,3])
          }
      }
      else{
        colons <- ones(merged_final[,3])
      }
    }
    # Detect if successful exchanged
    if(TRUE %in% colons){
      E <- 1
    }
    return(E)
  }
  else{
    warning(paste0("PA directory does not exist: ", dir))
  }
}