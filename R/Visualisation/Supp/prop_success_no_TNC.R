################################################################################
# Name: prop_success_no_TNC.R
# Author: Lucas Buffan (lucas.l.buffan@gmail.com)
# Goal: Display proportion of successful runs for each model x starting continent
#       combination with different levels of tropical occupancy (T.O.) within the
#       colonised continent.
################################################################################

library(tidyverse)

source("./R/useful/helper_functions.R")

## Individual plots ------------------------------------------------------------
for(thr in c(0.25, 0.5, 0.75, 0.9, 1)){
  for(model in c("M0", "M1", "M2", "M3")){
    for(start in c("North", "South")){
      # Get the runs showing tropical niche conservatism (TNC) in the area to colonise (constrained to tropical biome)
      rel_biome <- readRDS(paste0("./Data/Relative_biome_areas/", model, "_", start, "America_start_rel_area.RDS"))
      TNC_runs <- rel_biome$Run[which(rel_biome$Tropical >= thr)]
      # Open corresponding param_tbl, filter out non-exchanged + TNC
      param_tbl <- readRDS(paste0("./Data/Gen3sis_parameter_tables/", model, "/", start,
                                  "_America_parameters_EXTENDED_EXCH_AREA_DIV_DIST_biome_time.RDS"))
      param_tbl <- param_tbl %>% filter(exchanged != -1) # no crash
      param_tbl_exch_noTNC <- param_tbl %>% filter(exchanged == 1 &
                                                     Sim_index %in% paste0("Simulation_", TNC_runs) == FALSE)
      # Compute success proportion and corresponding CI
      suc_prop <- nrow(param_tbl_exch_noTNC) / nrow(param_tbl)
      # Increment plot_df
      if(model == "M0" & start == "North"){
        ## 1. Proportion of success (associated CI from binomial) ----------------
        plot_df_prop_success <- data.frame(Ori = paste0(start, " America"),
                                           Prop_success = suc_prop,
                                           Lower_CI = bino_CI(prop = suc_prop,
                                                              n = nrow(param_tbl),
                                                              alpha = 0.05,
                                                              what = "Lower"),
                                           Upper_CI = bino_CI(prop = suc_prop,
                                                              n = nrow(param_tbl),
                                                              alpha = 0.05,
                                                              what = "Upper"),
                                           Model = model)
      }
      else{
        plot_df_prop_success <- plot_df_prop_success %>% 
          add_row(Ori = paste0(start, " America"),
                  Prop_success = suc_prop,
                  Lower_CI = bino_CI(prop = suc_prop,
                                     n = nrow(param_tbl),
                                     alpha = 0.05,
                                     what = "Lower"),
                  Upper_CI = bino_CI(prop = suc_prop,
                                     n = nrow(param_tbl),
                                     alpha = 0.05,
                                     what = "Upper"),
                  Model = model)
      }
    }
  }
  # Plot it and save for each Tropical Occupancy (T.O.) threshold
  prop_plot <- plot_df_prop_success %>% 
    ggplot(aes(x = Ori, y = Prop_success)) +
    geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.15) +
    geom_point(aes(fill = Ori), colour = "black", pch = 23, size = 1.5) +
    scale_fill_manual(values = c("#fb6a4aff", "#66c2a4ff")) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1.12), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    facet_wrap(.~Model, ncol = 2) +
    ggtitle(paste0("TNC threshold: ", thr)) +
    labs(x = "Ancestral continent", y = "Proportion of successful exchange") +
    theme(axis.title = element_text(size = 7.5),
          axis.text = element_text(size = 5),
          axis.line = element_line(linewidth = 0.3, color = "black"),
          legend.position = "none",
          panel.background = element_rect(fill = "white", colour = "black"),
          panel.grid.major = element_line(linewidth = 0.1, color = "grey50"),
          panel.grid.minor = element_line(linewidth = 0.1, color = "grey50"),
          plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
          plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
          strip.background = element_rect(fill = "#DDE6F5"))
  
  ggsave(paste0("./Figures/MS/Supp/PropSuccess_TNC_thresholds/Figure_PropSuccess_TNC_thr_", thr, ".png"), 
         plot = prop_plot, height = 120, width = 110, dpi = 600, units = "mm")
  
}

## Panel -----------------------------------------------------------------------
for(thr in c(0.25, 0.5, 0.75, 0.9, 1)){
  for(model in c("M0", "M1", "M2", "M3")){
    for(start in c("North", "South")){
      # Get the runs showing tropical niche conservatism (TNC) in the area to colonise (constrained to tropical biome)
      rel_biome <- readRDS(paste0("./Data/Relative_biome_areas/", model, "_", start, "America_start_rel_area.RDS"))
      TNC_runs <- rel_biome$Run[which(rel_biome$Tropical >= thr)]
      # Open corresponding param_tbl, filter out non-exchanged + TNC
      param_tbl <- readRDS(paste0("./Data/Gen3sis_parameter_tables/", model, "/", start,
                                  "_America_parameters_EXTENDED_EXCH_AREA_DIV_DIST_biome_time.RDS"))
      param_tbl <- param_tbl %>% filter(exchanged != -1) # no crash
      param_tbl_exch_noTNC <- param_tbl %>% filter(exchanged == 1 &
                                                     Sim_index %in% paste0("Simulation_", TNC_runs) == FALSE)
      # Compute success proportion and corresponding CI
      suc_prop <- nrow(param_tbl_exch_noTNC) / nrow(param_tbl)
      # Increment plot_df
      if(model == "M0" & start == "North" & thr == 0.25){
        ## 1. Proportion of success (associated CI from binomial) ----------------
        plot_df_prop_success <- data.frame(Ori = paste0(start, " America"),
                                           Prop_success = suc_prop,
                                           Lower_CI = bino_CI(prop = suc_prop,
                                                              n = nrow(param_tbl),
                                                              alpha = 0.05,
                                                              what = "Lower"),
                                           Upper_CI = bino_CI(prop = suc_prop,
                                                              n = nrow(param_tbl),
                                                              alpha = 0.05,
                                                              what = "Upper"),
                                           Model = model,
                                           Threshold = paste0("T.O. threshold: ", thr))
      }
      else{
        plot_df_prop_success <- plot_df_prop_success %>% 
          add_row(Ori = paste0(start, " America"),
                  Prop_success = suc_prop,
                  Lower_CI = bino_CI(prop = suc_prop,
                                     n = nrow(param_tbl),
                                     alpha = 0.05,
                                     what = "Lower"),
                  Upper_CI = bino_CI(prop = suc_prop,
                                     n = nrow(param_tbl),
                                     alpha = 0.05,
                                     what = "Upper"),
                  Model = model,
                  Threshold = paste0("T.O. threshold: ", thr))
      }
    }
  }
}

# Plot it and save for each Tropical Occupancy (T.O.) threshold
prop_plot <- plot_df_prop_success %>% 
  mutate(Threshold = factor(Threshold, levels = paste0("T.O. threshold: ", c(1, 0.9, 0.75, 0.5, 0.25)))) %>%
  ggplot(aes(x = Ori, y = Prop_success)) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.15) +
  geom_point(aes(fill = Ori), colour = "black", pch = 23, size = 1.5) +
  scale_fill_manual(values = c("#fb6a4aff", "#66c2a4ff")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.12), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  facet_grid(Threshold~Model) +
  labs(x = "Ancestral continent", y = "Proportion of successful exchange") +
  theme(axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 5),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major = element_line(linewidth = 0.1, color = "grey50"),
        panel.grid.minor = element_line(linewidth = 0.1, color = "grey50"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        strip.background = element_rect(fill = "#DDE6F5"))

ggsave("./Figures/MS/Supp/PropSuccess_TNC_thresholds/Panel_PropSuccess_TNC.png", 
       plot = prop_plot, height = 200, width = 200, dpi = 600, units = "mm")
