################################################################################
# Name: 8-Area_and_div.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Quantify the proportion of colonised area occupied by a species and the 
#       total diversity present in this area => run on BIGMEM
################################################################################

library(terra)
source("helper_functions.R")

args <- commandArgs(trailingOnly = T)
mdl <- as.character(args[1]) # M0, M1, ...
start <- as.character(args[2]) # North or South

# Function assessing whether a species occupies a grid from a gridded landscape
species_occupies <- function(col, df){ # df is the clipped occupancy dataframe
  ifelse(length(unique(as.numeric(df[,col]))) == 2, TRUE, FALSE)
}
# Function assessing whether a grid cell of a landscape coded in binary presence/absence is occupied or not
cell_occupied <- function(row, df){ # df is the clipped occupancy dataframe
  ifelse(length(unique(as.numeric(df[row,]))) == 2, TRUE, FALSE)
}

# Function to retrieve either of the two target metrics
get_area_div <- function(run,
                         model,
                         what = c("area", "diversity"),
                         last_step, # last step of the simulation (if set to zero, simulation went until the end)
                         ancestral_area = c("North", "South")){ # where the ancestral species started (either North or South America)
  # -----------------
  # if `what` = "area", returns the proportion of colonised area occupied
  # if `what` = "diversity", returns the number of species in the colonised area
  # -----------------
  if(ancestral_area == "North"){
    mask <- vect("../Data/Shapefile_masks/South_America_cut.shp")
  }
  if(ancestral_area == "South"){
    mask <- vect("../Data/Shapefile_masks/North_America_cut.shp")
  }
  # Open gridded pa mat of the final step as an sf object
  of <- readRDS(paste0("../Outputs/", model, "/", ancestral_area, "_America_start/config_", model, 
                       "_", ancestral_area, "_America_run_", run, "/pa_matrices/pa_t_", last_step, ".rds"))
  of <- data.frame(of)
  grid_vect <- vect(of, geom = c("x", "y"), crs = crs(mask))
  # Intersect with mask
  grid_clipped <- grid_vect[mask, ]
  grid_clipped_df <- as.data.frame(grid_clipped)
  # Diversity in colonised area
  if(what == "diversity"){
    sp_occ <- sapply(1:ncol(grid_clipped_df), FUN = species_occupies, df = grid_clipped_df)
    div <- ncol(grid_clipped_df[, sp_occ])
    return(div)
  } 
  # Proportion of area colonised (in pixels)
  if(what == "area"){
    avail_area <- nrow(grid_clipped_df)
    cell_occ <- sapply(1:nrow(grid_clipped_df), FUN = cell_occupied, df = grid_clipped_df)
    grid_clipped_df_occ <- grid_clipped_df[cell_occ, ]
    occ_area <- nrow(grid_clipped_df_occ)
    prop_col_area <- occ_area / avail_area
    return(prop_col_area)
  }
}

# Execute
# Open the corresponding param table
ptbl <- read.table(paste0("../Data/param_tables/", mdl, "/", start, "_America_parameters_EXTENDED_EXCH.txt"),
                   header = T)
# Select runs that resulted in a successful exchange (i.e., flagged 1 in the `exchanged` column)
success_runs <- c(1:nrow(ptbl))[which(ptbl$exchanged == 1)]
# Add target metric columns and fill them
  # Proportion of colonised area
ptbl$prop_col_area <- -1
prop_col_area_vect <- c()
for(sr in success_runs){
  prop_col_area_vect <- c(prop_col_area_vect, get_area_div(run = sr,
                                                           model = mdl,
                                                           what = "area",
                                                           last_step = ptbl$final_timestep[sr],
                                                           ancestral_area = start))
}
ptbl$prop_col_area[success_runs] <- prop_col_area_vect
  # Diversity in the colonised area
ptbl$div_col <- -1    
div_col_area_vect <- c()
for(sr in success_runs){
  div_col_area_vect <- c(div_col_area_vect, get_area_div(run = sr,
                                                         model = mdl,
                                                         what = "diversity",
                                                         last_step = ptbl$final_timestep[sr],
                                                         ancestral_area = start))
}
ptbl$diversity_col_area[success_runs] <- div_col_area_vect
# Save
write.tbl.std(ptbl, 
              paste0("../Data/param_table/", mdl, "/", start, "_America_parameters_EXTENDED_EXCH_AREA_DIV.txt"))

