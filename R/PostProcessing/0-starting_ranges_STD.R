################################################################################
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Aim: Plot starting ranges for all runs when standardising distances to isthmus
#      (to run on BIGMEM).
################################################################################

library(raster)

args <- commandArgs(trailingOnly = T) # don't specify mdl as starting ranges are shared
nruns <- args[1]  # 500

## Standardised --------------------------------------------------------------
# Initialise
run <- 1
path_to_pa <- paste0("../Outputs_Eq_Dist/M0/North_America_start/Config_", run, "/config_M0_North_America_run_", run, "/pa_matrices/")
pa_0 <- readRDS(paste0(path_to_pa, "pa_t_499.rds")) # oldest time step numbered last in gen3sis

# Loop across remaining runs
for(run in 2:nruns){
  path_to_pa <- paste0("../Outputs_Eq_Dist/M0/North_America_start/Config_", run, "/config_M0_North_America_run_", run, "/pa_matrices/")
  m0 <- readRDS(paste0(path_to_pa, "pa_t_499.rds"))
  pa_0[which(m0[,3] == 1),3] <- run
}

# Save xyz tbl
saveRDS(pa_0, "../Outputs_Eq_Dist/all_starting_ranges/North_America_start_xyz_DIST_STD.RDS")

# Rasterise, plot and save
# r0 <- rasterFromXYZ(pa_0)
# pdf(paste0("../Outputs/all_starting_ranges/", from, "_America_start.pdf"))
# plot(r0)
# dev.off()