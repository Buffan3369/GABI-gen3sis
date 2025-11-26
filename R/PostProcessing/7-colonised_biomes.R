################################################################################
# Name: 5e-StartBiome.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Get the relative proportion of colonised area per each biome
################################################################################

library(raster)

## Function to create composite 1 0 column (1 if presence in the cell, 0 otherwise) --------
composite_col <- function(mat){
  if(ncol(mat) > 3){
    concat <- apply(X = mat[, 3:ncol(mat)], FUN = sum, MARGIN = 1)
    comp <- sapply(X = concat, FUN = function(x){ifelse(x >= 1, 1, 0)})
  }
  else{
    comp <- mat[, 3]
  }
  return(comp)
}

## Assess the final colonised area per biomes ----------------------------------
get_col_biomes_end <- function(i, start, model, biome){
  
  # Get PA matrix for final time step
  p_tbl <- readRDS(paste0("../Data/param_tables/", model, "/", start, 
                          "_America_parameters_EXTENDED_EXCH_AREA_DIV_DIST_biome_time.RDS"))
  t_end <- p_tbl$final_timestep[i]
  end_mat <- readRDS(paste0("../Outputs/", model, "/", start, "_America_start/config_", model,
                            "_", start, "_America_run_", i, "/pa_matrices/pa_t_", t_end, ".rds"))
  end_mat <- as.data.frame(end_mat)
  
  # Mask area of origin (we are only interested in the colonised area)
  if(start == "North"){
    mask <- shapefile("../Data/Shapefile_masks/South_America_cut.shp")
  }
  if(start == "South"){
    mask <- shapefile("../Data/Shapefile_masks/North_America_cut.shp")
  }
  r_last <- rasterFromXYZ(end_mat[, 1:3])
  extracted <- raster::extract(r_last, mask, df = TRUE, cellnumbers = TRUE)
  xy_col <- xyFromCell(r_last, extracted$cell)
  end_mat_col <- merge(end_mat, xy_col, by = c("x", "y"))
  
  # Create composite df (xyz where z is a one-hot encoded vector equal to 1 when the corresponding cell is occupied)
  comp <- composite_col(end_mat_col)
  end_xyz <- cbind(end_mat_col[, 1:2], comp)
  xy_end_occupied <- end_xyz[which(end_xyz$comp == 1), 1:2]
  
  # Get the colonised biomes
  biome_end <- readRDS(paste0("../Data/pClimate/KG_biomes/KG_", t_end*10, "ka_broadclass.RDS"))
  extracted_PA <- raster::extract(biome_end, xy_end_occupied)
  
  # Assess the relative proportion of colonised area for the desired biome
  if(biome == "Tropical"){
    rel_area <- length(which(extracted_PA == 1)) / length(extracted_PA)
  }
  else if(biome == "Arid"){
    rel_area <- length(which(extracted_PA == 2)) / length(extracted_PA)
  }
  else if(biome == "Temperate"){
    rel_area <- length(which(extracted_PA == 3)) / length(extracted_PA)
  }
  else if(biome == "Cold"){
    rel_area <- length(which(extracted_PA == 4)) / length(extracted_PA)
  }
  else if(biome == "Polar"){
    rel_area <- length(which(extracted_PA == 5)) / length(extracted_PA)
  }
  else{
    stop("Unknown biome specified")
  }
  return(rel_area)
}

## Execute ---------------------------------------------------------------------
for(start in c("North", "South")){
  for(model in c("M0", "M1", "M2", "M3")){
    p_tbl <- readRDS(paste0("../Data/param_tables/", model, "/", start, 
                            "_America_parameters_EXTENDED_EXCH_AREA_DIV_DIST_biome_time.RDS"))
    succ_runs <- which(p_tbl$exchanged == 1)
    rel_biome_area <- data.frame(Run = succ_runs,
                                 Tropical = sapply(X = succ_runs, FUN = get_col_biomes_end,
                                                   start = start, model = model, biome = "Tropical"),
                                 Arid = sapply(X = succ_runs, FUN = get_col_biomes_end,
                                                   start = start, model = model, biome = "Arid"),
                                 Temperate = sapply(X = succ_runs, FUN = get_col_biomes_end,
                                                   start = start, model = model, biome = "Temperate"),
                                 Cold = sapply(X = succ_runs, FUN = get_col_biomes_end,
                                                   start = start, model = model, biome = "Cold"),
                                 Polar = sapply(X = succ_runs, FUN = get_col_biomes_end,
                                                   start = start, model = model, biome = "Polar"))
    
    saveRDS(rel_biome_area,
            paste0("../Data/Relative_biome_areas/", model, "_", start, "America_start_rel_area.RDS"))
  }
}
