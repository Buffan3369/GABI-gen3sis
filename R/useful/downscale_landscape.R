################################################################################
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Downscale gen3sis landscape to a smaller time step
################################################################################

library(DDD)

args <- commandArgs(trailingOnly=TRUE)
# 1. path towards landscape we want to downscale
# 2. downscaling step

## Landscape RDS (be careful, numbering goes from 1 to N) ----------------------
ldsc <- readRDS(paste0(args[1], "/landscapes.rds"))
step <- as.numeric(args[2])
new_ldsc <- list()
for(feature in names(ldsc)){
  tmp <- ldsc[[feature]]
  tmp <- tmp[unique(c(1,2, seq((step/step)+2, length(tmp), step)))]
  colnames(tmp) <- c("x", "y", 1:(ncol(tmp)-2))
  new_ldsc[[feature]] <- tmp
}
# Create path towards downscaled landscape if necessary and save RDS
spl <- strsplit(args[1], split = "/")[[1]]
down_path <- paste(spl[1:(length(spl)-1)], collapse = "/")
down_path <- paste0(down_path, "/downscaled_landscape_", step)
#print(down_path)
if(!file.exists(down_path)){dir.create(path = down_path)}
saveRDS(new_ldsc, paste0(down_path, "/landscapes.rds"))

## Distance matrices  (be careful, numbering goes from 0 to N-1) ---------------
# Full
full_path <- paste0(args[1], "/distances_full/")
full_path_downscaled <- paste0(down_path, "/distances_full/")
if(!file.exists(full_path_downscaled)){dir.create(path = full_path_downscaled)}
c_mat_full <- list.files(full_path)
time_steps <- unique(c(seq(step-1, length(c_mat_full), step), 
                       (length(c_mat_full)-1) )) # oldest one, being indexed as maxTime - 1, as starts from 0

retained_full <- paste0("distances_full_", time_steps, ".rds")
equiv_name_down_full <- paste0("distances_full_", ((time_steps+1)/step), ".rds") # to match ldsc

sapply(X = 1:length(retained_full),
       FUN = function(x){
         file.copy(paste(full_path, retained_full[x], sep = "/"),
                   paste(full_path_downscaled, equiv_name_down_full[x], sep = "/"),
                   recursive = FALSE, copy.mode = TRUE)})
# And add first time step
file.copy(paste0(full_path, "/distances_full_0.rds"),
          paste0(full_path_downscaled, "/distances_full_0.rds"))

# Local
local_path <- paste0(args[1], "/distances_local/")
local_path_downscaled <- paste0(down_path, "/distances_local/")
if(!file.exists(local_path_downscaled)){ dir.create(path = local_path_downscaled)}
c_mat_local <- list.files(local_path)

retained_local <- paste0("distances_local_", time_steps, ".rds")
equiv_name_down_local <- paste0("distances_local_", ((time_steps+1)/step), ".rds") # to match ldsc

sapply(X = 1:length(retained_local),
       FUN = function(x){
         file.copy(paste(local_path, retained_local[x], sep = "/"),
                   paste(local_path_downscaled, equiv_name_down_local[x], sep = "/"),
                   recursive = FALSE,  copy.mode = TRUE)})
# And add first time step
file.copy(paste0(local_path, "/distances_local_0.rds"),
          paste0(local_path_downscaled, "/distances_local_0.rds"))

## Small README not to get lost ------------------------------------------------
str <- paste0("Warning, the original time resolution is 1ky.\n",
              "Here, we downscaled to a factor ", step, ".\n",
              "Time steps are therefore of ", step, "ky.")
writeLines(str, paste0(down_path, "/README.txt"))
