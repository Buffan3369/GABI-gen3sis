################################################################################
# Goal: Run configs generated for the M0 scenario (fully-connected landscape, 
#   species distribution, ecology and evolution only mediated by climate 
#   constraints, i.e. prec + temp). 
################################################################################

library(gen3sis)
library(future.apply)

args <- commandArgs(trailingOnly=TRUE)



## Function to run a config file by its index ---------------------------------- 
run_sim_i <- function(i){
  # Path towards the i-th config file
  config_dir <- list.files(paste0(args[1], "/Config_", i),
                           full.names = TRUE, pattern = ".R")[[1]]
  # Path towards landscape
  input_dir <- "../../Data/Landscapes/downscaled_landscape_10/"
  # Path towards output directory
  output_dir <- paste0(paste0(args[1], "/Config_", i))
  # Run simulation
  sim <- run_simulation(config = config_dir,
                        landscape = input_dir,
                        output_directory = output_dir)
}

## Set-up multithreading and apply the function --------------------------------
plan(multicore, workers = as.numeric(args[2]))
future_sapply(X = 1:as.numeric(args[3]), 
              FUN = run_sim_i)
plan(sequential)
