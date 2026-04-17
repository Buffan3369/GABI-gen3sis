################################################################################
# Name: 2-PostProcessing_MASTER.R
# Author: Lucas Buffan (lucas.l.buffan@gmail.com)
# Goal: MasterScript \nAdding additional features related to simulations after 
#       being run to the param table.
#       /!\ Meant to be run on BigMem10 /!\
################################################################################

args <- commandArgs(trailingOnly=TRUE)
# -------------------------------------------------------- #
# args[1] : path towards parameter table
# args[2] : number of generations
# args[3] : number of replicates
# args[4] : model (e.g. M0, M1)
# args[5] : region (North or South)
# args[6] : standardised distances to isthmus? (set to TRUE if yes)
# args[7] : Oscillayers-based simulations? (set to TRUE if yes)
# args[8] : Simulations with expanded thermal niche? (set to TRUE if yes)
# args[9] : path to and name of the extended parameter table
# -------------------------------------------------------- #

source("./helper_functions.R")

## Open parameter table --------------------------------------------------------
init_params <- read.table(args[1],
                          header = T)

## ------------------------------------------- ##
## 1. Add descriptive features post-simulation ##
## ------------------------------------------- ##
source("./2a-NewParams.R")
params <- Add_new_params(param_tbl       = init_params,
                         n_gen           = as.numeric(args[2]),
                         n_sim           = as.numeric(args[3]),
                         mdl             = args[4],
                         start_continent = args[5],
                         eq_dist         = as.logical(args[6]),
                         oscill          = as.logical(args[7]),
                         expanded        = as.logical(args[8]))
cat("\nAdding New Descriptive Parameters: DONE\n\n")

## ------------------------------------------------------------------ ##
## 2. Indicate whether each run resulted in a successful colonisation ##
## ------------------------------------------------------------------ ##
source("./2b-ColAssessor.R")
params$exchanged <- sapply(X       = 1:as.numeric(args[3]),
                           FUN     = transfert,
                           n_sim   = as.numeric(args[2]),
                           mdl     = args[4],
                           from    = args[5],
                           eq_dist = as.logical(args[6]),
                           oscill  = as.logical(args[7]),
                           expanded= as.logical(args[8]))
cat("\nAdding Successful Colonisations: DONE\n\n")

## ------------------------------------------------------------------------------- ##
## 3. Assessing the area occupied by colonisers and the diversity within this area ##
## ----------------------------------------------------------------------------- - ##
source("./2c-AreaDiv.R")
  # Select runs that resulted in a successful exchange (i.e., flagged 1 in the `exchanged` column)
success_runs <- c(1:nrow(params))[which(params$exchanged == 1)]
  # Add target metric columns and fill them
params$prop_col_area <- -1
params$abs_col_area <- -1
params$div_col <- -1
  # Loop across successful runs
for(sr in success_runs){
  print(sr)
  # -------------------------- #
  # Proportion of colonised area
  # -------------------------- #
  area <- get_area_div(run            = sr,
                       model          = args[4],
                       what           = "area",
                       last_step      = params$final_timestep[sr],
                       ancestral_area = args[5],
                       eq_dist        = as.logical(args[6]),
                       oscill         = as.logical(args[7]),
                       expanded       = as.logical(args[8]))
  params$prop_col_area[sr] <- ifelse(length(area) == 0, -1, area)
  # -------------------------- #
  # Absolute colonised area
  # -------------------------- #
  abs_area <- get_area_div(run            = sr,
                           model          = args[4],
                           what           = "absolute_area",
                           last_step      = params$final_timestep[sr],
                           ancestral_area = args[5],
                           eq_dist        = as.logical(args[6]),
                           oscill         = as.logical(args[7]),
                           expanded       = as.logical(args[8]))
  params$abs_col_area[sr] <- ifelse(length(abs_area) == 0, -1, abs_area)
  # ----------------------------- #
  # Diversity in the colonised area
  # ----------------------------- #
  div <- get_area_div(run            = sr,
                      model          = args[4],
                      what           = "diversity",
                      last_step      = params$final_timestep[sr],
                      ancestral_area = args[5],
                      eq_dist        = as.logical(args[6]),
                      oscill         = as.logical(args[7]),
                      expanded       = as.logical(args[8]))
  params$div_col[sr] <- div
}
cat("\nAdding Propotion and Absolute Colonised Area and Diversity within this Area : DONE\n\n")


## --------------------------------------------------------------------- ##
## 4. Distance of the simulation starting point to the isthmus of Panama ##
## --------------------------------------------------------------------- ##
source("./2d-DistIsthm.R")
# Assess distance between simulation starting point and isthmus
dists <- sapply(X       = 1:as.numeric(args[3]),
                FUN     = get_dist_start,
                model   = args[4],
                start   = args[5],
                n_sim   = as.numeric(args[2]),
                eq_dist = as.logical(args[6]),
                oscill  = as.logical(args[7]),
                expanded= as.logical(args[8]))
params$dist_to_isthmus <- dists
cat("\nAdding distance of ancestral species to Isthmus: DONE\n\n")


## -------------------------------------------------------------- ##
## 5. Assess Köppen-Geiger biome where initial species originates ##
## -------------------------------------------------------------- ##
source("./2e-StartBiome.R")
# Assess distance between simulation starting point and isthmus
biomes <- sapply(X       = 1:as.numeric(args[3]),
                 FUN     = get_biome_start,
                 model   = args[4],
                 n_sim   = as.numeric(args[2]),
                 start   = args[5],
                 eq_dist = as.logical(args[6]),
                 oscill  = as.logical(args[7]),
                 expanded= as.logical(args[8]))
params$start_biome <- biomes
cat("\nAdding Starting Biome: DONE\n\n")


## Save ------------------------------------------------------------------------
write_tbl_std(params, args[9])
