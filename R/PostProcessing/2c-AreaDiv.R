################################################################################
# Name: 5c-AreaDiv.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Function quantifying the proportion of colonised area occupied by a 
#       species and the total diversity present in this area
################################################################################

library(terra)
source("helper_functions.R")

# Function assessing whether a species occupies a grid from a gridded landscape
species_occupies <- function(col, df){ # df is the clipped occupancy dataframe
  ifelse( (1 %in% unique(as.numeric(df[,col]))) , TRUE, FALSE)
}
# Function assessing whether a grid cell of a landscape coded in binary presence/absence is occupied or not
cell_occupied <- function(row, df){ # df is the clipped occupancy dataframe
  ifelse( (1 %in% unique(as.numeric(df[row,]))) , TRUE, FALSE)
}

# Function to retrieve either of the two target metrics
#          /!\ APPLY TO SUCCESSFUL RUNS /!\ 
get_area_div <- function(run,
                         model,
                         what = c("area", "absolute_area", "diversity"),
                         last_step, # last step of the simulation (if set to zero, simulation went until the end)
                         ancestral_area = c("North", "South"), # where the ancestral species started (either North or South America)
                         eq_dist = FALSE,
                         oscill = FALSE){ 
  # -----------------
  # if `what` = "area", returns the proportion of colonised area occupied
  # if `what` = "absolute_area", returns the absolute size of the colonised area (in number of pixels)
  # if `what` = "diversity", returns the number of species in the colonised area
  # -----------------
  if(ancestral_area == "North"){
    mask <- vect("../../Data/Shapefile_masks/South_America_cut.shp")
  }
  if(ancestral_area == "South"){
    mask <- vect("../../Data/Shapefile_masks/North_America_cut.shp")
  }
  # -----------------
  # Open gridded pa mat of the final step as an sf object
  # Differential path whether considering equal distances or not
  # -----------------
  suff <- ""
  if(eq_dist){
    suff <- "_Eq_Dist"
  }
  else if(oscill){
    suff <- "_Oscillayers"
  }
  of <- readRDS(paste0("../../Outputs", suff, "/", model, "/", ancestral_area, "_America_start/Config_", i,
                       "config_", model, "_", ancestral_area, "_America_run_", run, "/pa_matrices/pa_t_", 
                       last_step, ".rds"))
  of <- data.frame(of)
  grid_vect <- vect(of, geom = c("x", "y"), crs = crs(mask))
  # Intersect with mask
  grid_clipped <- grid_vect[mask, ]
  grid_clipped_df <- as.data.frame(grid_clipped)
  # Diversity in colonised area
  if(what == "diversity"){
    sp_occ <- sapply(1:ncol(grid_clipped_df), FUN = species_occupies, df = grid_clipped_df)
    div <- ifelse(length(which(sp_occ) == TRUE) > 1, 
                  ncol(grid_clipped_df[, sp_occ]), # does not work in case `grid_clipped_df` is unidimensional (meaning diversity in the foreign continent = 1 species)
                  1) # obviously, as this function is meant to be applied to successful runs
    return(div)
  }
  else{
    avail_area <- nrow(grid_clipped_df)
    cell_occ <- sapply(1:nrow(grid_clipped_df), FUN = cell_occupied, df = grid_clipped_df)
    grid_clipped_df_occ <- grid_clipped_df[cell_occ, ]
    if(ncol(grid_clipped_df) > 1){
      occ_area <- nrow(grid_clipped_df_occ)
    }
    else{
      occ_area <- length(grid_clipped_df_occ)
    }
    # Proportion of area colonised (in pixels)
    if(what == "area"){
      prop_col_area <- occ_area / avail_area
      return(prop_col_area)
    }
    # Absolute colonised area
    if(what == "absolute_area"){
      return(occ_area)
    }
  }
}
