################################################################################
# Name: 7c-RTT_spatial.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Assess macroevol rates through time (RTT) in both areas (North and 
#       South America).
##############################################################################

library(raster)

## Accessory function ----------------------------------------------------------
not.zero <- function(vect){
  a <- unique(vect)
  if(length(a) == 1 && a[1] == 0){
    return(FALSE)
  }
  else{
    return(TRUE)
  }
}

phylo_sum_NS <- function(run, 
                         from = c("North", "South"), # region of origin of the ancestral species 
                         where = c("North", "South")){ # region where we want to assess number of originations and extinctions
  # Number of simulation steps for the run i
  
  # pa_dir <- paste0("../Outputs/M1_eq_area/", from, "_America_start/config_M1_",
  #                  from, "_America_run_", run, "/pa_matrices")
  pa_dir <- paste0("./Results/M1_eq_area/", from, "_America_start/config_M1_South_America_run_", run, "/pa_matrices/")
  
  n_iter <- length(list.files(pa_dir))
  # Coordinates of the cells falling in the region of interest
  
#  xy_region <- readRDS(paste0("../Data/pa_cells_North_South/xy_", where, "_Am.RDS"))
  xy_region <- readRDS(paste0("./Data/pa_cells_North_South/xy_", where, "_Am.RDS"))
  
    # Loop
  sp_prev <- c() # will contain the species present at the previous time step
  speciations <- rep(0, n_iter) # Store speciation events at every time step
  extinctions <- rep(0, n_iter) # Store extinction events at every time step
  for(i in 1:n_iter){
    t <- 500 - i
    pa_t <- readRDS(paste0(pa_dir, "/pa_t_", t, ".rds"))
    # Subset region of interest
    pa_t_region <- merge(pa_t, xy_region, by = c("x", "y"))
    # Remove columns with absent species (i.e., columns of zeros)
    if(ncol(pa_t) > 3){
      abs <- apply(X = pa_t_region[,3:ncol(pa_t)], FUN = not.zero, MARGIN = 2)
      }
    else{abs <- not.zero(pa_t[,3])}
    pa_t_region_present <- pa_t_region[, c(TRUE, TRUE, # we keep the first 2 cols (x,y)
                                            abs)]
    # (local) Extinctions?
    sp <- colnames(pa_t_region_present[, -c(1,2)])
    extinct <- which(sp_prev %in% sp == FALSE) # which species present at the last step is no longer among regional species list
    if(length(extinct) >= 1){extinctions[i] <- length(extinct)}
    # Speciation
    new_species <- which(sp %in% sp_prev == FALSE) # which species appeared at this time step?
    if(length(new_species) >= 1){speciations[i] <- length(sp) - length(sp_prev)}
  }
  final_df <- data.frame(time = seq(499:(499-n_iter+1)),
                         speciations = speciations,
                         extinctions = extinctions)
  saveRDS(object = final_df,
          file = paste0("~/Bureau/phylo_sum-RUN_", 
                        run, "-", from, "_America_START-", where, "_America.RDS"))
}


phylo_sum_NS(run = 265,
             from = "South",
             where = "North")

phylo_sum_NS(run = 265,
             from = "South",
             where = "South")
