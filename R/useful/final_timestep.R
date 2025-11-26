################################################################################
# Name: final_timestep.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Accessory script to get the final timestep of each successful simulation
################################################################################

library(tidyverse)

finals <- c()

for(start in c("North", "South")){
  for(model in c("M0", "M1", "M2", "M3")){
    p_tbl <- readRDS(paste0("./Data/Gen3sis_parameter_tables/", model, "/", start, "_America_parameters_EXTENDED_EXCH_AREA_DIV_DIST_biome_time.RDS"))
    p_tbl_exch <- p_tbl %>% filter(exchanged == 1)
    finals <- c(finals, unique(p_tbl_exch$final_timestep))
  }
}

finals <- sort(unique(finals))
saveRDS(finals, "./Data/Gen3sis_parameter_tables/final_timesteps.RDS")