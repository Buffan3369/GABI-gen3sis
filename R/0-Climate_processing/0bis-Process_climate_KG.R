################################################################################
# Name: 0bis-Process_climate_KG.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Functions to process monthly climate rasters to obtain biome maps.
#       NOT MADE FOR RSTUDIO
################################################################################

library(terra)

source("~/Documents/KoppenGeiger_inR/kg_reclass_raster.R")

get_mothly_clim_at_t <- function(month, # labelled from 1 to 12
                                 t, # t goes forward, from 1 (5Ma) to 5000 (present) 
                                 what = c("temperature", "precipitation")){
  if(month < 10){
    month <- paste0("0", month)
  }
  clim_rast_list <- readRDS(paste0("../../Data/PALEO_PGEM-bioclim/monthly_mean_rasters/mean_", what,
                                   "_list_", month, "M_Americas.RDS"))
  return(clim_rast_list[t])
}

reclass_rasters <- function(t){
  # Get raster list
  Temp_rasters <- lapply(X = 1:12, FUN = get_mothly_clim_at_t, t = t, what = "temperature")
  Prec_rasters <- lapply(X = 1:12, FUN = get_mothly_clim_at_t, t = t, what = "precipitation")
  # Rasetrise
  Temp_rasters <- terra::rast(Temp_rasters)
  Prec_rasters <- terra::rast(Prec_rasters)
  # Reclassify according to KG criteria
  clim_reclass <- kg_reclass_raster(Temp = Temp_rasters,
                                    Prec = Prec_rasters)
  return(clim_reclass)
}
