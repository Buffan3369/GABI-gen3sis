library(tidyverse)
source("./helper_functions.R")
source("./2c-AreaDiv.R")

for(model in c("M0", "M1", "M2", "M3")){
  for(start in c("North", "South")){
    params <- readRDS(paste0("../Data/param_tables/", model, "/", start, "_America_parameters_EXTENDED_EXCH_AREA_DIV_DIST_biome_time.RDS"))
    success_runs <- c(1:nrow(params))[which(params$exchanged == 1)]
    params$abs_col_area <- -1
    for(sr in success_runs){
      abs_area <- get_area_div(run = sr,
                               model = model,
                               what = "absolute_area",
                               last_step = params$final_timestep[sr],
                               ancestral_area = start,
                               eq_dist = F)
      params$abs_col_area[sr] <- ifelse(length(abs_area) == 0, -1, abs_area)
    }
    saveRDS(params, 
            paste0("../Data/param_tables/", model, "/", start, "_America_parameters_EXTENDED_EXCH_ABS_AREA_DIV_DIST_biome_time.RDS"))
  }
}
