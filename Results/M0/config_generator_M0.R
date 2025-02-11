################################################################################
# Goal: Generate configs for the M0 scenario (fully-connected landscape, 
#   species distribution, ecology and evolution only mediated by climated 
#   constraints, i.e. prec + temp)
################################################################################

library(randtoolbox)
source("./R/useful/helper_functions.R")

## Number of replicates --------------------------------------------------------
n <- 100

## Function mapping Sobol' sequences to a desired parameter range linearly -----
#   (adapted from Hagen et al. (2021), Skeels et al. (2023a,b), ...)
linMap <- function(x, from, to, rnd = NA) {
  rescaled <- (x - min(x)) / max(x - min(x)) * (to - from) + from
  if(!is.na(round)){
    return(round(rescaled, digits = rnd))
  }
}

for(start_region in c("North_America", "South_America")){
## Initialise 8-dim Sobol' sequence --------------------------------------------
  param_df <- data.frame(sobol(n = n, dim = 8))
  colnames(param_df) <- c("S",          # Divergence threshold above which speciation occurs
                          "omega_T",    # Strength of environmental filtering for temperature
                          "omega_P",    # Same for precipitations
                          "sigma_e",    # Rate of environmental niche evolution
                          "disp_shape", # Shape of the dispersal kernel (Weibull distribution)
                          "disp_scale", # Scale of the dispersal kernel
                          "decay",      # Decay parameter for extirpation (i.e., local extinction) probability
                          "inflection") # Inflection parameter for extirpation probability
  
## Rescale parameters to desired range -----------------------------------------
  param_df$S <-          linMap(param_df$S, from = 0.75, to = 1.5, rnd = 3)
  param_df$omega_T <-    linMap(param_df$omega_T, from = 0.01, to = 0.1, rnd = 3)
  param_df$omega_P <-    linMap(param_df$omega_T, from = 0.05, to = 0.3, rnd = 3)
  param_df$sigma_e <-    linMap(param_df$sigma_e, from = 0.005, to = 0.1, rnd = 3)
  param_df$disp_shape <- linMap(param_df$disp_shape, from = 0.5, to = 3, rnd = 3)
  param_df$disp_scale <- linMap(param_df$disp_scale, from = 0.1, to = 2, rnd = 3)
  param_df$decay <-      linMap(param_df$decay, from = -1.5, to = 1.5, rnd = 3)
  param_df$disp_shape <- linMap(param_df$disp_shape, from = 0.5, to = 2, rnd = 3)
  
## Add fixed parameters --------------------------------------------------------
  param_df$seed = 1:n                # random seed, for replicate reproducibility
  param_df$max_species = 10000       # max number of species (above which simulation aborpts)
  param_df$max_coex_sp = 1000        # max nb. of species per cell
  param_df$start_species = 1         # number of species to start with
  param_df$init_ab = 500             # initial abundance of starting species
  param_df$grid_cell_distance <- 111 # grid cell distance (1x1° lat in km at the equator)
  
## Save parameter table --------------------------------------------------------
  write.table.std(param_df, 
                  paste0("./Data/Gen3sis_parameter_tables/M0/",start_region, "_parameters.txt"))
## Generate config files -------------------------------------------------------
  for(i in 1:n){
    params <- param_df[i, ]
  }  
}
