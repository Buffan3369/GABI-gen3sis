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
# args[4] : path towards where outputs are stored
# args[5] : model (e.g. M0, M1)
# args[6] : region (North or South)
# args[7] : path to and name of the extended parameter table
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
                         n_gen           = args[2],
                         n_sim           = args[3],
                         sim_dir         = args[4],
                         mdl             = args[5],
                         start_continent = args[6])
cat("\nAdding New Descriptive Parameters: DONE\n\n")

## ------------------------------------------------------------------ ##
## 2. Indicate whether each run resulted in a successful colonisation ##
## ------------------------------------------------------------------ ##
source("./2b-ColAssessor.R")
params$exchanged <- sapply(X = 1:as.numeric(args[2]),
                           FUN = transfert,
                           mdl = args[5],
                           from = args[6])
cat("\nAdding Successful Colonisations: DONE\n\n")


## ------------------------------------------------------------------------------- ##
## 3. Assessing the area occupied by colonisers and the diversity within this area ##
## ----------------------------------------------------------------------------- - ##
source("./2c-AreaDiv.R")
  # Select runs that resulted in a successful exchange (i.e., flagged 1 in the `exchanged` column)
success_runs <- c(1:nrow(params))[which(params$exchanged == 1)]
  # Add target metric columns and fill them
params$prop_col_area <- -1
params$div_col <- -1
  # Loop across successful runs
for(sr in success_runs){
  # -------------------------- #
  # Proportion of colonised area
  # -------------------------- #
  area <- get_area_div(run = sr,
                       model = args[5],
                       what = "area",
                       last_step = params$final_timestep[sr],
                       ancestral_area = args[6])
  params$prop_col_area[sr] <- ifelse(length(area) == 0, -1, area)
  # ----------------------------- #
  # Diversity in the colonised area
  # ----------------------------- #
  div <- get_area_div(run = sr,
                      model = args[5],
                      what = "diversity",
                      last_step = params$final_timestep[sr],
                      ancestral_area = args[6])
  params$div_col[sr] <- div
}
cat("\nAdding Colonised Area Surface and Diversity within this Area : DONE\n\n")


## --------------------------------------------------------------------- ##
## 4. Distance of the simulation starting point to the isthmus of Panama ##
## --------------------------------------------------------------------- ##
source("./2d-DistIsthm.R")
# Assess distance between simulation starting point and isthmus
dists <- sapply(X     = 1:as.numeric(args[3]),
                FUN   = get_dist_start,
                model = args[5],
                start = args[6])
params$dist_to_isthmus <- dists
cat("\nAdding distance of ancestral species to Isthmus: DONE\n\n")


## -------------------------------------------------------------- ##
## 5. Assess Köppen-Geiger biome where initial species originates ##
## -------------------------------------------------------------- ##
source("./2e-StartBiome.R")
# Assess distance between simulation starting point and isthmus
biomes <- sapply(X     = 1:as.numeric(args[3]),
                 FUN   = get_biome_start,
                 model = args[5],
                 start = args[6])
params$start_biome <- biomes
cat("\nAdding Starting Biome: DONE\n\n")


## Save ------------------------------------------------------------------------
write_tbl_std(params, args[7])
