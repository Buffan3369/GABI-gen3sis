################################################################################
# Goal: Generate configs for the M0 scenario starting from North America after
#       standardising distances of starting species to isthmus of Panama with 
#       simulations starting in South America
################################################################################

library(randtoolbox)
source("./R/useful/helper_functions.R")

## Initial parameters ----------------------------------------------------------
n <- 500 # number of replicates
start_region  <- "North_America"

## Initialise 5-dim Sobol' sequence --------------------------------------------
param_df <- data.frame(cbind(paste0("Simulation_", 1:n), sobol(n = n, dim = 6)))
colnames(param_df) <- c("Sim_index",  # Simulation index
                        "S",          # Divergence threshold above which speciation occurs
                        "omega_T",    # Strength of environmental filtering for temperature
                        "omega_P",    # Same for precipitations
                        "sigma_e",    # Rate of environmental niche evolution
                        "disp_shape", # Shape of the dispersal kernel (Weibull distribution)
                        "disp_scale") # Scale of the dispersal kernel
                    
## Rescale parameters to desired range -----------------------------------------
param_df$S <-          linMap(as.numeric(param_df$S), from = 1, to = 3, rnd = T, dgts = 3)
param_df$omega_T <-    linMap(as.numeric(param_df$omega_T), from = 0.01, to = 0.1, rnd = T, dgts = 3)
param_df$omega_P <-    linMap(as.numeric(param_df$omega_P), from = 0.05, to = 0.3, rnd = T, dgts = 3)
param_df$sigma_e <-    linMap(as.numeric(param_df$sigma_e), from = 0.005, to = 0.15, rnd = T, dgts = 3)
param_df$disp_shape <- linMap(as.numeric(param_df$disp_shape), from = 0.5, to = 3, rnd = T, dgts = 3)
param_df$disp_scale <- linMap(as.numeric(param_df$disp_scale), from = 0.1, to = 1, rnd = T, dgts = 3)

## Add fixed parameters --------------------------------------------------------
param_df$seed <- 1:n                # random seed, for replicate reproducibility
param_df$divergence_factor <- 0.05  # how much divergence populations accumulate across iterations
param_df$max_species <- 12000       # max number of species (above which simulation aborpts)
param_df$max_coex_sp <- 1000        # max nb. of species per cell
param_df$start_species <- 1         # number of species to start with
param_df$init_ab <- 500             # initial abundance of starting species
param_df$grid_cell_distance <- 111  # grid cell distance (1x1° lat in km at the equator)

## Save parameter table --------------------------------------------------------
if(!dir.exists("./Data/Gen3sis_parameter_tables/Equal_dist/M0")){dir.create("./Data/Gen3sis_parameter_tables/Equal_dist/M0")}
write.tbl.std(param_df, 
              paste0("./Data/Gen3sis_parameter_tables/Equal_dist/M0/", start_region, "_parameters_EQ_Dist.txt"))

## Access the distances to isthmus chosen for South American species -----------
param_tbl_SA_M0 <- read.table("./Data/Gen3sis_parameter_tables/M0/South_America_parameters_EXTENDED_EXCH_AREA_DIV_DIST_biome.txt",
                              header = TRUE)
init_dists <- param_tbl_SA_M0$dist_to_isthmus
# If some distances couldn't be computed, we fill them by sampling from a normal with empirically-derived mean and sd
if(length(unique(is.na(init_dists))) > 1){
  is_na_dist <- which(is.na(init_dists))
  for(idx in is_na_dist){
    init_dists[idx] <- rnorm(n = 1, 
                             mean = mean(param_tbl_SA_M0$dist_to_isthmus, na.rm = T), 
                             sd = sd(param_tbl_SA_M0$dist_to_isthmus, na.rm = T))
  }
}
param_df$dist_to_isthm <- init_dists

## Generate config files -------------------------------------------------------
out <- paste0("./Data/Gen3sis_configs/Equal_distance/M0/", start_region, "_start")
for(i in 1:n){
  # Select settings for simulation i
  params <- param_df[i, ]
  # Create directory where config will be stored
  out_dir <- paste0(out, "/Config_", i)
  dir.create(out_dir)
  # Edit config template
  config_i <- readLines(paste0("./Data/Gen3sis_configs/Equal_distance/M0/config_M0_North_America-EQ_DIST_template.R"))
  config_i <- gsub("params\\$S", params$S, config_i)
  config_i <- gsub("params\\$divergence_factor", params$divergence_factor, config_i)
  config_i <- gsub("params\\$omega_T", params$omega_T, config_i)
  config_i <- gsub("params\\$omega_P", params$omega_P, config_i)
  config_i <- gsub("params\\$sigma_e", params$sigma_e, config_i)
  config_i <- gsub("params\\$disp_shape", params$disp_shape, config_i)
  config_i <- gsub("params\\$disp_scale", params$disp_scale, config_i)
  config_i <- gsub("params\\$seed", params$seed, config_i)
  config_i <- gsub("params\\$max_species", params$max_species, config_i)
  config_i <- gsub("params\\$max_coex_sp", params$max_coex_sp, config_i)
  config_i <- gsub("params\\$start_species", params$start_species, config_i)
  config_i <- gsub("params\\$init_ab", params$init_ab, config_i)
  config_i <- gsub("params\\$grid_cell_distance", params$grid_cell_distance, config_i)
  config_i <- gsub("params\\$dist_to_isthm", params$dist_to_isthm, config_i)
  # Write config for run i
  writeLines(config_i, paste0(out_dir, "/config_M0_", start_region, "_run_", i, ".R"))
}  

