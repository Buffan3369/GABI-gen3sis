library(raster)
library(tidyverse)
mask <- shapefile("./Data/Shapefile_masks/South_America_cut.shp")

args <- commandArgs(trailingOnly=TRUE)

model <- args[1] # model
start <- args[2] # starting landmass

# Open corresponding param table to retrive the indices of successful runs
param_tbl <- read.table(paste0("../Data/param_tbl/", model, "/", start, 
                               "_America_parameters_EXTENDED_EXCH_AREA_DIV_DIST_biome.txt"),
                        sep = "\t", header = TRUE)
succ_runs <- as.numeric(rownames(param_tbl)[which(param_tbl$exchanged == 1)])

PA_matrices <- list.files("~/Bureau/pa_matrices")

# Flag matrix names to order them forward-in-time
for(i in 0:499){
  idx <- which(PA_matrices == paste0("pa_t_", i, ".rds"))
  if(500-i < 10){
    PA_matrices[idx] <- paste0("00", (500-i), "-", PA_matrices[idx])
  }
  else if(500-i < 100){
    PA_matrices[idx] <- paste0("0", (500-i), "-", PA_matrices[idx])
  }
  else{
    PA_matrices[idx] <- paste0((500-i), "-", PA_matrices[idx])
  }
}
PA_matrices <- PA_matrices[order(PA_matrices)]

# Coordinates of the cells of any PA matrix falling within the mask
pa_499_name <- strsplit(PA_matrices[1], split = "-")[[1]][2]
pa_499 <- readRDS(paste0("~/Bureau/pa_matrices/", pa_499_name))
r_last <- rasterFromXYZ(pa_499, crs = crs(mask))
extracted <- raster::extract(r_last, mask, df = TRUE, cellnumbers = TRUE)
xy_mask <- xyFromCell(r_last, extracted$cell)

# Go across PA matrices for all timesteps to record timing of the first colonisation
i <- 1 # we don't care about the first time step, no colonisation possible
exch <- F
while(exch == F){
  i <- i + 1
  t <- 500-i
  PA_t_name <- strsplit(PA_matrices[i], split = "-")[[1]][2]
  PA_t <- readRDS(paste0("~/Bureau/pa_matrices/", PA_t_name))
  # merge with coordinates of cells falling within the mask
  merged_final <- merge(PA_t, xy_mask, by = c("x", "y"))
  if(ncol(merged_final) > 3){ # if more than one species represented
    composite <- apply(X = merged_final[,3:ncol(merged_final)], FUN = sum, MARGIN = 1)
  }
  else{
    composite <- merged_final[,3]
  }
  # if we have at least one species in the target landmass
  if(length(unique(composite)) > 1){
    exch <- T
  }
}


r <- rasterFromXYZ(PA_t, crs = crs(mask))
plot(r)
