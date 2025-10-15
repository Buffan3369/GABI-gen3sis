################################################################################
# Name: initial_range_map_panel.R
# Author: Lucas Buffan
# Contact: lucas.l.buffan@gmail.com
# Aim: Plotting initial range maps
################################################################################

library(tidyverse)

## All runs --------------------------------------------------------------------

U_TBL <- data.frame(lon = NA, lat = NA, run = NA, start = NA, model = NA)

for(model in c("M0", "M1", "M2", "M3")){
  # Tables
  NthAm_start <- data.frame(readRDS(paste0("./Results/starting_ranges/", model, "_North_America_start_xyz.RDS")))
  colnames(NthAm_start) <- c("lon", "lat", "run")
  NthAm_start$start <- "North America"
  
  NthAm_start_std <- data.frame(readRDS(paste0("./Results/starting_ranges/", model, "_North_America_start_xyz_DIST_STD.RDS")))
  colnames(NthAm_start_std) <- c("lon", "lat", "run")
  NthAm_start_std$start <- "North America (standardised)"
  
  SthAm_start <- data.frame(readRDS(paste0("./Results/starting_ranges/", model, "_South_America_start_xyz.RDS")))
  colnames(SthAm_start) <- c("lon", "lat", "run")
  SthAm_start$start <- "South America"
  
  # Union tables
  union_tbl <- rbind.data.frame(NthAm_start, NthAm_start_std, SthAm_start)
  union_tbl$model <- model
  U_TBL <- rbind.data.frame(U_TBL, union_tbl)
}

U_TBL <- U_TBL[-1,]

# Plot
init_maps <- U_TBL %>% 
  ggplot(aes(x = lon, y = lat, fill = run)) +
  geom_tile() +
  scale_fill_viridis_c() +
  facet_grid(model~start) +
  labs(x = "Longitude", y = "Latitude", fill = "Simulation\n index") +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 7),
        legend.title = element_text(size = 7, hjust = 0.5),
        legend.text = element_text(size = 5),
        legend.key.size = unit(4, "mm"),
        strip.text = element_text(size = 9))

ggsave("./Figures/MS/Supp/starting_ranges/Initial_ranges_All_runs.png", 
       plot = init_maps, dpi = 600, height = 200, width = 200, units = "mm")

## Only successful runs --------------------------------------------------------
for(mdl in c("M0", "M1", "M2", "M3")){
  
  # Open corresponding maps
  NthAm_start <- data.frame(readRDS(paste0("./Results/starting_ranges/", model, "_North_America_start_xyz.RDS")))
  colnames(NthAm_start) <- c("lon", "lat", "run")
  NthAm_start$start <- "North America"
  
  NthAm_start_std <- data.frame(readRDS(paste0("./Results/starting_ranges/", model, "_North_America_start_xyz_DIST_STD.RDS")))
  colnames(NthAm_start_std) <- c("lon", "lat", "run")
  NthAm_start_std$start <- "North America (standardised)"
  
  SthAm_start <- data.frame(readRDS(paste0("./Results/starting_ranges/", model, "_South_America_start_xyz.RDS")))
  colnames(SthAm_start) <- c("lon", "lat", "run")
  SthAm_start$start <- "South America"
  
  # NthAm start
  recap_tbl_NthAm <- read.table(paste0("./Data/Gen3sis_parameter_tables/", mdl, "/North_America_parameters_EXTENDED_EXCH_AREA_DIV_DIST_biome.txt"),
                             sep = "\t", header = T)
  success_idx_NthAm <- as.numeric(rownames(recap_tbl_NthAm)[recap_tbl_NthAm$exchanged == 1])
  tmp_NthAm_start <- NthAm_start %>% 
    mutate(run = sapply(X = run,
                        FUN = function(x){
                          if(x %in% success_idx_NthAm){
                            return(x)
                          }
                          else{
                            return(0)
                          }
                        })) %>% 
    mutate(model = mdl)
  
  # SthAm start
  recap_tbl_SthAm <- read.table(paste0("./Data/Gen3sis_parameter_tables/", mdl, "/South_America_parameters_EXTENDED_EXCH_AREA_DIV_DIST_biome.txt"),
                             sep = "\t", header = T)
  success_idx_SthAm <- as.numeric(rownames(recap_tbl_SthAm)[recap_tbl_SthAm$exchanged == 1])
  tmp_SthAm_start <- SthAm_start %>%
    mutate(run = sapply(X = run,
                        FUN = function(x){
                          if(x %in% success_idx_SthAm){
                            return(x)
                          }
                          else{
                            return(0)
                          }
                        })) %>% 
    mutate(model = mdl)
  
  # NthAm standardised start
  recap_tbl_NthAm_std <- read.table(paste0("./Data/Gen3sis_parameter_tables/Equal_dist/", mdl, "/North_America_parameters_EqDist_EXTENDED_EXCH_AREA_DIV_DIST_biome.txt"),
                                    sep = "\t", header = T)
  success_idx_NthAm_std <- as.numeric(rownames(recap_tbl_NthAm_std)[recap_tbl_NthAm_std$exchanged == 1])
  tmp_NthAm_std_start <- NthAm_start_std %>% 
    mutate(run = sapply(X = run,
                        FUN = function(x){
                          if(x %in% success_idx_NthAm_std){
                            return(x)
                          }
                          else{
                            return(0)
                          }
                        })) %>% 
    mutate(model = mdl)
  
  if(mdl == "M0"){
    u_tbl_success <- rbind.data.frame(tmp_NthAm_start, tmp_SthAm_start, tmp_NthAm_std_start)
  }
  else{
    u_tbl_success <- rbind.data.frame(u_tbl_success, tmp_NthAm_start, tmp_SthAm_start, tmp_NthAm_std_start)
  }
}

# Plot

init_maps_success <- u_tbl_success %>% 
  ggplot(aes(x = lon, y = lat, fill = run)) +
  geom_tile() +
  scale_fill_viridis_c() +
  facet_grid(model~start) +
  labs(x = "Longitude", y = "Latitude", fill = "Simulation\n index") +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 7),
        legend.title = element_text(size = 7, hjust = 0.5),
        legend.text = element_text(size = 5),
        legend.key.size = unit(4, "mm"),
        strip.text = element_text(size = 9))

ggsave("./Figures/MS/Supp/starting_ranges/Initial_ranges_successful_runs.png", 
       plot = init_maps_success, dpi = 600, height = 200, width = 200, units = "mm")
