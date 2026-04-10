################################################################################
# Goal: Run configs generated for each scenario starting from North America,
#       after standardising distances to isthmus with South America.
################################################################################


library(gen3sis)
library(future.apply)


args <- commandArgs(trailingOnly=TRUE)
# args[1]: model (M0, M1, ...)
# args[2]: number of threads to monopolise (max 40)
# args[3]: number of simulations we want to run (500)


## Function to run a config file by its index ---------------------------------- 
run_sim_i <- function(i){
  # Path towards the i-th config file
  config_dir <- paste0("../Outputs_Eq_Dist/", args[1], "/North_America_start/Config_", i,  "/config_", args[1], "_North_America_run_", i, ".R")
  # Path towards landscape
  input_dir <- "../Data/Landscapes/downscaled_landscape_10/"
  # Path towards output directory
  output_dir <- paste0("../Outputs_Eq_Dist/", args[1], "/North_America_start/Config_", i, "/")
  # Run simulation
  sim <- run_simulation(config = config_dir,
                        landscape = input_dir,
                        output_directory = output_dir)
}

## Set-up multithreading and apply the function --------------------------------
plan(multicore, workers = as.numeric(args[2]))
future_sapply(X = 1:args[3], FUN = run_sim_i) # Ancestor from North America
plan(sequential)



