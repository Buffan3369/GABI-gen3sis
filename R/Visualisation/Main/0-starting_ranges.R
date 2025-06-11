################################################################################
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Aim: Plot starting ranges for all runs (to run on BIGMEM).
################################################################################

library(raster)

args <- commandArgs(trailingOnly = T)
mdl <- args[1]    # M0, M1, ...
from <- args[2]   # North or South
nruns <- args[3]  # 500

# Initialise
run <- 1
path_to_pa <- paste0("../Outputs/", mdl, "/", from, "_America_start/config_", mdl, "_", from, "_America_run_", run, "/pa_matrices/")
pa_0 <- readRDS(paste0(path_to_pa, "pa_t_499.rds")) # oldest time step numbered last in gen3sis

# Loop across remaining runs
for(run in 2:nruns){
  path_to_pa <- paste0("../Outputs/", mdl, "/", from, "_America_start/config_", mdl, "_", from, "_America_run_", run, "/pa_matrices/")
  m0 <- readRDS(paste0(path_to_pa, "pa_t_499.rds"))
  pa_0[which(m0[,3] == 1),3] <- run
}

# Rasterise, plot and save
r0 <- rasterFromXYZ(pa_0)
pdf(paste0("../Outputs/all_starting_ranges/", mdl, "_", from, "_America_start.pdf"))
plot(r0)
dev.off()