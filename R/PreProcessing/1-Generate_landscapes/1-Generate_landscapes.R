################################################################################
# Name: 1-Generate_landscapes.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Genrate landscape based on MAT and MAP layers throughout the last 5Myrs
################################################################################

library(gen3sis)

## Temperature portion of our landscape ----------------------------------------
#temp_list <- readRDS("./Data/Gen3sis_landscapes/MAT_list_Americas.RDS")
temp_list <- readRDS("./Data/pClimate/MAT_list_Americas.RDS") # Bigmem


## Precipitations portion of our landscape -------------------------------------
#prec_list <- readRDS("./Data/Gen3sis_landscapes/MAP_list_Americas.RDS")
prec_list <- readRDS("./Data/pClimate/MAP_list_Americas.RDS") # Bigmem


## Generate a gen3sis landscape ------------------------------------------------
# Cost function (twice as high for water as for earth)
cost_function <- function(source, habitable_src, dest, habitable_dest) {
  if(!all(habitable_src, habitable_dest)) {
    return(2/1000)
  } else {
    return(1/1000)
  }
}
# Landscape properely speaking
landscape_list <- list(temp = temp_list,
                       prec = prec_list)
create_input_landscape(landscapes = landscape_list,
                       timesteps = as.character(1:5000),
                       cost_function = cost_function,
                       directions = 8,
                       output_directory = "./Data/Landscapes/Fully_connected",
                       overwrite_output = T,
                       crs = "+proj=longlat +datum=WGS84 +no_defs",
                       calculate_full_distance_matrices = T)
