################################################################################
# Name: 6-N_colonisations_assessor.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: This scripts assesses whether a simulation led to successful interchange.
#       Meant to be run on bm10.
################################################################################

library(raster)
library(dplyr)
source("helper_functions.R")

args <- commandArgs(trailingOnly=T)
# args[1] : model (M0, M1, ...)

## Define transfert function ---------------------------------------------------
transfert <- function(i,  # returns 1 if run i resulted in a successful transfert
                      from = c("North", "South")){ # from = ancestral area
  E <- 0 # exchange metric
  # Define mask
  if(from == "North"){
    mask <- shapefile("../Data/Shapefile_masks/South_America_cut.shp")
  }
  if(from == "South"){
    mask <- shapefile("../Data/Shapefile_masks/North_America_cut.shp")
  }
  # Define directory
  dir <- paste0("../Outputs/", args[1], "/", from, "_America_start/config_", args[1], "_", from, "_America_run_", i, "/pa_matrices")
  if(dir.exists(dir)){
    # Open the presence/absence matrix of the last time step and ratserise it (1 ancestor => perfect xyz table)
    t_end <- 500 - length(list.files(paste0(dir)))
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

## SOUTH -> NORTH --------------------------------------------------------------
param_tbl_south <- read.table(paste0("../Data/param_tables/", args[1], "/South_America_parameters_EXTENDED.txt"),
                              header = T)
param_tbl_south$exchanged <- sapply(X = 1:500,
                                    FUN = transfert,
                                    from = "South")

write.tbl.std(param_tbl_south,
              paste0("../Data/param_tables/", args[1], "/South_America_parameters_EXTENDED_EXCH.txt"))

## NORTH -> SOUTH --------------------------------------------------------------
param_tbl_north <- read.table(paste0("../Data/param_tables/", args[1], "/North_America_parameters_EXTENDED.txt"),
                        header = T)
param_tbl_north$exchanged <- sapply(X = 1:500,
                                    FUN = transfert,
                                    from = "North")
write.tbl.std(param_tbl_north,
              paste0("../Data/param_tables/", args[1], "/North_America_parameters_EXTENDED_EXCH.txt"))




#### NUMBER OF SUCCESSFUL COLONISATIONS (TO BE IMPROVED) ####
# /!\ Issue: cannot make the difference between a lineage effectively crossing
#     and the descendence of this lineage, not crossing in fact.
# /!\ How to deal with extinctions?


## Loop across species P/A matrices through time to get the final number of colonisations
# n_col <- 0
# dir <- "./Results/M1/South_America_start/config_M1_South_America_run_1/pa_matrices/"
# # list of S->N colons
# colons_prev <- c(FALSE) # at t-1
# colons <- c(FALSE) # at t
# # list of species
# sp_list_prev <- c("X1") # at t-1
# sp_list <- c("X1") # at t
# n_steps <- length(list.files(dir))
# for(t in seq(499, (499-n_steps+1), -1)){
#   # Open Presence/Absence matrix
#   pa_t <- readRDS(paste0(dir, "pa_t_", t, ".rds"))
#   pa_t <- data.frame(pa_t)
#   # Update list of species
#   sp_list <- colnames(pa_t)
#   # Isolate North America by matching extracted cells with their original values
#   pa_t_NA <- data.frame(merge(pa_t, xy_NA, by = c("x", "y")))
#   # Have some species reached North America?
#   if(ncol(pa_t_NA) == 3){ 
#     colons <- ones(pa_t_NA[,3])
#   }
#   else{
#     colons <- apply(X = pa_t_NA[,3:ncol(pa_t_NA)], MARGIN = 2, FUN = ones)
#   }
#   # Check whether some detected "colons" in fact result from a northern lineage that gave birth to a previously unknown lineage
#   if(length(which(colons)) > length(which(colons_prev))){
#     cat(paste0("Colonisation at t = ", t, " ! \n"))
#     n_col <- n_col + ( length(which(colons)) - length(which(colons_prev)) )
#   }
#   colons_prev <- colons
# }
