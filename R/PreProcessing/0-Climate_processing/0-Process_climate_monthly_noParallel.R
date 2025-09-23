################################################################################
# Name: 0-Process_climate_monthly.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Process MAT and MAP rasters before using them to design gen3sis inputs.
#       /!\ MEANT TO BE RUN ON BIGMEM /!\
################################################################################

library(DDD)
source("./0-Process_rasters.R")

## Vector mask only encompassing continental Americas --------------------------
americas <- shapefile("../../Data/Shapefile_masks/raw_mask.shp")

############################# MONTHLY AVERAGES #################################
args <- commandArgs(trailingOnly=TRUE)
i <- as.numeric(args[1]) # index of the month

month <- i
if(month < 10){
  month <- paste0("0", month)
}
## Monthly temperatures ------------------------------------------------------
Monthly_temp <- read.table(paste0("../../Data/PALEO_PGEM-bioclim/monthly_mean/monthly_temperature", month, "M.txt"), header = T)
sapply(X = 3:ncol(Monthly_temp),
       FUN = process_raster,
       mask = americas, climate_dataset = Monthly_temp, what = "temperature", month = i)
## Monthly precipitations ----------------------------------------------------
Monthly_prec <- read.table(paste0("../../Data/PALEO_PGEM-bioclim/monthly_mean/monthly_precipitation", month, "M.txt"), header = T)
sapply(X = 3:ncol(Monthly_prec),
       FUN = process_raster,
       mask = americas, climate_dataset = Monthly_prec, what = "precipitation", month = i)
