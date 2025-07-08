################################################################################
# Name: 9-dist_start_to_isthm.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Assess the distance between simulation starting point and the isthmus of
#       Panama => run on BIGMEM
################################################################################

library(raster)
library(sp)
library(spaths)

# Open a mask to get CRS
SA_mask <- shapefile("../Data/Shapefile_masks/South_America_cut.shp")

# Function to assess the distance between sim° starting point & isthmus -------- 
get_dist_start <- function(i, start, model){
  init_mat <- readRDS(paste0("../Outputs/", model, "/", start, "_America_start/", 
                             "config_", model, "_", start, "_America_run_", i, "/pa_matrices/pa_t_499.rds"))
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