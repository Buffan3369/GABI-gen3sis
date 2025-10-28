## Generate presence/absence matrices across time for each simulated species

library(future.apply)

args <- commandArgs(trailingOnly=TRUE)

get_sp_distrib <- function(sp, t_end, pa_dir, phy_path, out_dir){
  # Open the `phy.txt` file
  phy <- read.table(phy_path, sep = "\t", header = T)
  # Initialise species distrib through time table
  pa_end <- readRDS(paste0(pa_dir, "/pa_t_", t_end, ".rds"))
  sp_end <- pa_end[, as.character(sp)]
  sp_df <- data.frame(t_0 = sp_end)
  # Loop between t_end and the time of origination of the species
  t_ori <- phy$Speciation.Time[phy$Descendent == sp]
  for(t_i in t_end:t_ori){
    pa_t_i <- readRDS(paste0(pa_dir, "/pa_t_", t_i, ".rds"))
    sp_df[, paste0("t_", t_i)] <- pa_t_i[, as.character(sp)]
  }
  saveRDS(sp_df, paste0(out_dir, "/Species_", sp, ".rds"))
}

apply_to_run <- function(run, model, start){
  # Directory to corresponding folder
  dir_to_folder <- paste0("../Outputs/", model, "/", start, "_America_start/config_", 
                          model, "_", start, "_America_run_", run)
  # Get the last time step of the run
  LF <- list.files(paste0(dir_to_folder, "/pa_matrices"))
  t_end <- 500-length(LF)
  pa_end <- readRDS(paste0(dir_to_folder, "/pa_matrices/pa_t_", t_end, ".rds"))
  # Create directory where output distrib per species will be stored
  if(dir.exists(paste0(dir_to_folder, "/pa_per_sp")) == F){
    dir.create(paste0(dir_to_folder, "/pa_per_sp"))
  }
  # Apply function to each species of the run
  sapply(X = colnames(pa_end)[-c(1,2)],
         FUN = get_sp_distrib,
         t_end = t_end,
         pa_dir = paste0(dir_to_folder, "/pa_matrices"),
         phy_path = paste0(dir_to_folder, "/phy.txt"),
         out_dir = paste0(dir_to_folder, "/pa_per_sp"))
}

for(model in c("M0", "M1", "M2", "M3")){
  for(start in c("North", "South")){
    # Retrieve the runs that resulted in a successful exchange
    param_tbl <- read.table(paste0("../Data/param_tables/", model, "/", start, 
                                   "_America_parameters_EXTENDED_EXCH_AREA_DIV_DIST_biome.txt"),
                            sep = "\t", header = T)
    succ_runs <- as.numeric(rownames(param_tbl)[which(param_tbl$exchanged == 1)])
    # Run function in parallel for each of these runs
    plan(multicore, workers = 40)
    future_sapply(X = succ_runs, FUN = apply_to_run, model = model, start = start)
    plan(sequential)
  }
}

