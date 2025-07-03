################################################################################
# Name: 9-dist_start_to_isthm.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Assess the distance between simulation starting point and the isthmus of
#       Panama => run on BIGMEM
# nb. Distances are assessed in x100m ...
################################################################################

library(raster)
library(gdistance)
library(sf)

# Open a mask to get CRS
SA_mask <- shapefile("../Data/Shapefile_masks/South_America_cut.shp")

# Function to assess the distance between sim° starting point & isthmus -------- 
get_dist_start <- function(i, start, model){
  cost_mat <- readRDS(paste0("../Outputs/", model, "/", start, "_America_start/", 
                             "config_", model, "_", start, "_America_run_", i, "/pa_matrices/pa_t_499.rds"))
  ## Check whether starting species existed ------------------------------------
  if(length(which(cost_mat[,3] == 1)) > 0){
    ## Define the two points we'll assess the distance between -------------------
    # Sample one of the starting cells
    spld <- sample(x = which(cost_mat[,3] == 1), size = 1)
    start_xy <- cost_mat[spld, c(1,2)]
    # Arbitrary point located near the Panamá isthmus
    isthm_coord <- c(-84, 10) 
    # Merge
    pts_df <- data.frame(rbind(start_xy, isthm_coord))
    colnames(pts_df) <- c("lon", "lat")
    # Convert to spatial point
    start_sp <- SpatialPoints(pts_df[1,], proj4string = crs(SA_mask))
    isthm_sp <- SpatialPoints(pts_df[2,], proj4string = crs(SA_mask))
    
    ## Rasterise cost matrix and create a transition object out of it ------------
    cost_rast <- rasterFromXYZ(cost_mat, crs = crs(SA_mask))
    # Transition object, defining the cost of moving from one cell to the other
    tr <- transition(cost_rast, 
                     directions = 8,
                     transitionFunction = function(x){
                       return(1/mean(x))
                     }
    )
    # Correct for geographic distortion (necessary to account for distortion in the map + unequal distances of moving vert/hzl and in diagonal)
    tr_corrected <- geoCorrection(tr, type = "c")
    
    ## Compute least-cost distance (i.e., minimal, as implies the least amount of steps) ---
    # Compute least-cost distance between two points
    cost_dist <- costDistance(tr_corrected, start_sp, isthm_sp)
    return(cost_dist)
  }
  else{
    return(NA)
  }
}

## Apply -----------------------------------------------------------------------
for(model in c("M0", "M1", "M2", "M3")){
  for(start in c("North", "South")){
    # Assess distance between simulation starting point and isthmus
    dists <- sapply(X = 1:500, FUN = get_dist_start, start = start, model = model)
    # Open param table and save it
    param_tbl <- read.table(paste0("../Data/param_tables/", model, "/", start, "_America_parameters_EXTENDED_EXCH_AREA_DIV.txt"),
                            header = T)
    param_tbl$dist_to_isthmus <- dists
    # Save
    write.table(param_tbl, paste0("../Data/param_tables/", model, "/", start, "_America_parameters_EXTENDED_EXCH_AREA_DIV_DIST.txt"))
  }
}