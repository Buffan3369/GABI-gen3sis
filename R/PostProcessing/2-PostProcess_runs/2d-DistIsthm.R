################################################################################
# Name: 5d-DistIsthm.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Assess the distance between simulation starting point and the isthmus of
#       Panama
################################################################################

library(raster)
library(sp)
library(spaths)

# Open a mask to get CRS
SA_mask <- shapefile("../../Data/Shapefile_masks/South_America_cut.shp")

# Function to assess the distance between sim° starting point & isthmus -------- 
get_dist_start <- function(i,
                           start,
                           model,
                           n_sim,
                           eq_dist = FALSE,
                           oscill = FALSE,
                           expanded = FALSE,        
                           w){
  # Open initial presence-absence matrix
  t_start <- n_sim-1
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
  init_mat <- readRDS(paste0("../../Outputs", suff, "/", model, "/", start, "_America_start/Config_", i,
                             "/config_", model, "_", start, "_America_run_", i, "/pa_matrices/pa_t_", 
                             t_start, ".rds"))
  # Check whether starting species existed
  if(length(which(init_mat[,3] == 1)) > 0){
    
    ## Define the two points we'll assess the distance between -------------------
    # Sample one of the starting cells
    spld <- sample(x = which(init_mat[,3] == 1), size = 1)
    start_xy <- init_mat[spld, c(1,2)]
    # Arbitrary point located near the Panamá isthmus
    isthm_coord <- c(-84, 10) 
    # Merge
    pts_df <- data.frame(rbind(start_xy, isthm_coord))
    colnames(pts_df) <- c("lon", "lat")
    # Convert to spatial point
    start_sp <- SpatialPoints(pts_df[1,], proj4string = crs(SA_mask))
    isthm_sp <- SpatialPoints(pts_df[2,], proj4string = crs(SA_mask))
    
    ## Compute shortest path between points on the rasterised landscape ------------
    init_rast <- rasterFromXYZ(init_mat, crs = crs(SA_mask))
    dist <- shortest_paths(rst = init_rast, origins = start_sp, destinations = isthm_sp)
    return(dist$distances / 1e3) # in km
  }
  else{
    return(NA)
  }
}
