################################################################################
# Name: select_snap.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Select snapshots of upscaled temp & prec from Oscillayers
################################################################################
temp_snap <- list()
prec_snap <- list()

temp <- readRDS("./MAT_list_Americas_Oscillayers.RDS")
prec <- readRDS("./MAP_list_Americas_Oscillayers.RDS")

idx <- c(1, 19, 99, 257, 299, 499)

for(t in idx){
  temp_snap[which(idx == t)] <- temp[[t]]
  prec_snap[which(idx == t)] <- prec[[t]]
}

saveRDS(temp_snap, "./upscaled_temperature_snapshots_Americas.RDS")
saveRDS(prec_snap, "./upscaled_precipitation_snapshots_Americas.RDS")