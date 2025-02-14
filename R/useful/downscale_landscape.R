################################################################################
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Downscale gen3sis landscape to a smaller time step
################################################################################

library(DDD)

args <- commandArgs(trailingOnly=TRUE)
# 1. path towards landscape we want to downscale
# 2. downscaling step

## Landscape RDS ---------------------------------------------------------------
ldsc <- readRDS(paste0(args[1], "/landscapes.rds"))
step <- as.numeric(args[2])
new_ldsc <- list()
for(feature in names(ldsc)){
  tmp <- ldsc[[feature]]
  tmp <- tmp[unique(c(1,2, seq(3, length(tmp), step), length(tmp)))]
  new_ldsc[[feature]] <- tmp
}
  # Create path towards downscaled landscape if necessary and save RDS
spl <- strsplit(args[1], split = "/")[[1]]
down_path <- paste(spl[1:(length(spl)-1)], collapse = "/")
down_path <- paste0(down_path, "/downscaled_landscape_", step)
if(!file.exists(down_path)){dir.create(path = down_path)}
saveRDS(new_ldsc, paste0(down_path, "/landscape.rds"))

## Distance matrices -----------------------------------------------------------
  # Full
full_path <- paste0(args[1], "/distances_full/")
full_path_downscaled <- paste0(down_path, "/distances_full/")
if(!file.exists(full_path_downscaled)){dir.create(path = full_path_downscaled)}
c_mat_full <- list.files(full_path)
time_steps <- unique(c(seq(0, length(c_mat_full), step),
                length(c_mat_full)-1)) # oldest one, being indexed as maxTime - 1, as starts from 0
retained_full <- paste0("distances_full_", time_steps, ".rds")
sapply(X = retained_full,
       FUN = function(x){
         file.copy(paste(full_path, x, sep = "/"),
                   paste(full_path_downscaled, x, sep = "/"),
                   recursive = FALSE, copy.mode = TRUE)})
  # Local
local_path <- paste0(args[1], "/distances_local/")
local_path_downscaled <- paste0(down_path, "/distances_local/")
if(!file.exists(local_path_downscaled)){ dir.create(path = local_path_downscaled)}
c_mat_local <- list.files(local_path)
retained_local <- paste0("distances_local_", time_steps, ".rds")
sapply(X = retained_local,
       FUN = function(x){
         file.copy(paste(local_path, x, sep = "/"),
                   paste(local_path_downscaled, x, sep = "/"),
                   recursive = FALSE,  copy.mode = TRUE)})

