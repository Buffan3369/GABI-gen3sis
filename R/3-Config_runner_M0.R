################################################################################
# Goal: Run configs generated for the M0 scenario (fully-connected landscape, 
#   species distribution, ecology and evolution only mediated by climate 
#   constraints, i.e. prec + temp). 
################################################################################

library(gen3sis)
library(future.apply)

## Function to run a config file by its index ---------------------------------- 
run_sim_i <- function(i, where = c("North", "South")){
  # Path towards the i-th config file
  config_dir <- paste0("../Outputs/M0/", where, "_America_start/Config_", i)
  # Path towards landscape
  input_dir <- "Data/Landscapes/Fully_connected"
  # Path towards output directory
  output_dir <- paste0("../Outputs/M0/", where, "_America_start/Config_", i)
  # Run simulation
  sim <- run_simulation(config = config_dir,
                        landscape = input_dir,
                        output_directory = output_dir)
}

## Set-up multithreading and apply the function --------------------------------
plan(multicore, workers = 33)
future_sapply(X = 1:100, FUN = run_sim_i, where = "North") # Ancestor from North America
future_sapply(X = 1:100, FUN = run_sim_i, where = "South") # Ancestor from South America
plan(sequential)
