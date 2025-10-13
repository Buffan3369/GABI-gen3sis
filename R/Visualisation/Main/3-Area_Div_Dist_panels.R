################################################################################
# Name: 3-Area_Div_Dist_panels.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Display postprocessing metrics
################################################################################

library(tidyverse)
source("./R/PostProcessing/4-Extract_distribs_with_moments.R")

################################################################################
#################### 1. PROPORTION OF SUCCESSFUL EXCHANGES #####################
################################################################################

## Unstandardised distances to isthmus -----------------------------------------
plot_df_prop_success <- readRDS("./Results/Exchanged_metrics/Prop_successful_exchange.RDS")

prop_plot2 <- plot_df_prop_success %>% 
  ggplot(aes(x = Ori, y = Prop_success)) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.15) +
  geom_point(aes(colour = Ori), size = 1.5) +
  scale_colour_manual(values = c("#fb6a4aff", "#66c2a4ff")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.12), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  labs(x = NULL, y = NULL) +
  facet_grid(.~Model) +
  labs(x = "Ancestral area", y = "Prop. successful exchange") +
  theme(axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 5),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major = element_line(linewidth = 0.1, color = "grey50"),
        panel.grid.minor = element_line(linewidth = 0.1, color = "grey50"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        strip.background = element_rect(fill = "#DDE6F5"))

ggsave("./Figures/prop_successful_exch/Panel_point_CI.pdf", 
       plot = prop_plot2, height = 80, width = 170, units = "mm")

ggsave("./Figures/prop_successful_exch/Panel_point_CI.png", 
       plot = prop_plot2, height = 80, width = 170, dpi = 600, units = "mm")


## Standardised distances to isthmus -------------------------------------------
plot_df_prop_success_std <- readRDS("./Results/Exchanged_metrics/Prop_successful_exchange_STD.RDS")

prop_plot2bis <- plot_df_prop_success_std %>% 
  ggplot(aes(x = Ori, y = Prop_success)) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.15) +
  geom_point(aes(colour = Ori), size = 1.5) +
  scale_colour_manual(values = c("#fb6a4aff", "#66c2a4ff")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.12), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  labs(x = NULL, y = NULL) +
  facet_grid(.~Model) +
  labs(x = "Ancestral area", y = "Prop. successful exchange") +
  theme(axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 5),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major = element_line(linewidth = 0.1, color = "grey50"),
        panel.grid.minor = element_line(linewidth = 0.1, color = "grey50"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        strip.background = element_rect(fill = "#DDE6F5"))

ggsave("./Figures/prop_successful_exch/Panel_point_CI-DIST_STD.pdf", 
       plot = prop_plot2bis, height = 80, width = 170, units = "mm")

ggsave("./Figures/prop_successful_exch/Panel_point_CI-DIST_STD.png", 
       plot = prop_plot2bis, height = 80, width = 170, dpi = 600, units = "mm")


################################################################################
############ 2. LARGE PANEL SUMMARISING THE 3 REMAINING METRICS ################
################################################################################

## Unstandardised Distances ##

## Load plot dataframes for the three metrics ----------------------------------
# Proportion of colonised area
plot_df_prop_col_area <- summarise_distrib(metric = "prop_col_area",
                                           what = "exchanged")
plot_df_prop_col_area <- plot_df_prop_col_area %>% 
  mutate(metric = "Proportion of colonised area") %>% 
  rename(value = "prop_col_area")
# Diversity in the colonised area
plot_df_div_col_area <- summarise_distrib(metric = "div_col",
                                          what = "exchanged",
                                          Log_transform = T)
plot_df_div_col_area <- plot_df_div_col_area %>% 
  mutate(metric = "Log(Diversty in the colonised area)") %>% 
  rename(value = "div_col")
# Distance to isthmus
plot_df_dist_isthmus <- summarise_distrib(metric = "dist_to_isthmus",
                                          what = "exchanged")
plot_df_dist_isthmus <- plot_df_dist_isthmus %>% 
  mutate(metric = "Distance to isthmus (km)") %>% 
  rename(value = "dist_to_isthmus")

## Merge them ------------------------------------------------------------------
PLOT_DF <- rbind.data.frame(plot_df_prop_col_area, plot_df_div_col_area, plot_df_dist_isthmus)
PLOT_DF$metric <- factor(PLOT_DF$metric, levels = c("Proportion of colonised area",
                                                    "Log(Diversty in the colonised area)",
                                                    "Distance to isthmus (km)"))

full_panel <- PLOT_DF %>% 
  ggplot(aes(x = start, y = value)) +
  geom_violin(adjust = .75, scale = "width", linewidth = 0.1, alpha = 0.5, aes(fill = factor(start))) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.1, linewidth = 0.2) +
  geom_point(aes(y = mean), size = 0.5) +
  scale_fill_manual(values = c("#fcbba1", "#ccece6")) +
  labs(x = NULL, y = NULL) +
  facet_grid(metric~model, scale = "free_y") +
  labs(x = "Ancestral area", y = NULL) +
  theme(axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 5),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        legend.position = "none",
        panel.background = element_rect(fill = "#F7F2E0", colour = "black"),
        panel.grid.major = element_line(linewidth = 0.1, color = "grey50"),
        panel.grid.minor = element_line(linewidth = 0.1, color = "grey50"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        strip.background = element_rect(fill = "#DDE6F5"))

ggsave("./Figures/MS/Main/Figure2/Figure2_baseline.pdf", 
       plot = full_panel, height = 200, width = 200, units = "mm")

ggsave("./Figures/MS/Main/Figure2/Figure2_baseline.png", 
       plot = full_panel, height = 200, width = 200, dpi = 600, units = "mm")



rm(plot_df_prop_col_area, plot_df_div_col_area, plot_df_dist_isthmus, PLOT_DF, full_panel)
## Standardised Distances ##

## Load plot dataframes for the three metrics ----------------------------------
# Proportion of colonised area
plot_df_prop_col_area_std <- summarise_distrib(metric = "prop_col_area",
                                               what = "exchanged",
                                               dist_std = T)
plot_df_prop_col_area_std <- plot_df_prop_col_area_std %>% 
  mutate(metric = "Proportion of colonised area") %>% 
  rename(value = "prop_col_area")
# Diversity in the colonised area
plot_df_div_col_area_std <- summarise_distrib(metric = "div_col",
                                              what = "exchanged",
                                              Log_transform = T,
                                              dist_std = T)
plot_df_div_col_area_std <- plot_df_div_col_area_std %>% 
  mutate(metric = "Log(Diversty in the colonised area)") %>% 
  rename(value = "div_col")
# Distance to isthmus
plot_df_dist_isthmus_std <- summarise_distrib(metric = "dist_to_isthmus",
                                              what = "exchanged",
                                              dist_std = T)
plot_df_dist_isthmus_std <- plot_df_dist_isthmus_std %>% 
  mutate(metric = "Distance to isthmus (km)") %>% 
  rename(value = "dist_to_isthmus")

## Merge them ------------------------------------------------------------------
PLOT_DF_STD <- rbind.data.frame(plot_df_prop_col_area_std,
                                plot_df_div_col_area_std,
                                plot_df_dist_isthmus_std)
PLOT_DF_STD$metric <- factor(PLOT_DF_STD$metric, levels = c("Proportion of colonised area",
                                                            "Log(Diversty in the colonised area)",
                                                            "Distance to isthmus (km)"))

full_panel_STD <- PLOT_DF_STD %>% 
  ggplot(aes(x = start, y = value)) +
  geom_violin(adjust = .75, scale = "width", linewidth = 0.1, alpha = 0.5, aes(fill = factor(start))) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.1, linewidth = 0.2) +
  geom_point(aes(y = mean), size = 0.5) +
  scale_fill_manual(values = c("#fcbba1", "#ccece6")) +
  labs(x = NULL, y = NULL) +
  facet_grid(metric~model, scale = "free_y") +
  labs(x = "Ancestral area", y = NULL) +
  theme(axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 5),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        legend.position = "none",
        panel.background = element_rect(fill = "#F7F2E0", colour = "black"),
        panel.grid.major = element_line(linewidth = 0.1, color = "grey50"),
        panel.grid.minor = element_line(linewidth = 0.1, color = "grey50"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        strip.background = element_rect(fill = "#DDE6F5"))

ggsave("./Figures/MS/Supp/metrics_std/panel_area_div_dist_std.pdf", 
       plot = full_panel_STD, height = 200, width = 200, units = "mm")

ggsave("./Figures/MS/Supp/metrics_std/panel_area_div_dist_std.png", 
       plot = full_panel_STD, height = 200, width = 200, dpi = 600, units = "mm")


rm(plot_df_prop_col_area_std, plot_df_div_col_area_std, plot_df_dist_isthmus_std, PLOT_DF_STD, full_panel_STD)
## Panel comparing distance to isthmus in unstandardised and standardised cases

plot_df_dist_isthmus <- summarise_distrib(metric = "dist_to_isthmus",
                                          what = "all",
                                          dist_std = F)
plot_df_dist_isthmus <- plot_df_dist_isthmus %>%  
  mutate(stand = "Unstandardised")


plot_df_dist_isthmus_std <- summarise_distrib(metric = "dist_to_isthmus",
                                              what = "all",
                                              dist_std = T)
plot_df_dist_isthmus_std <- plot_df_dist_isthmus_std %>% 
  mutate(stand = "Standardised")

plot_dist_df <- rbind.data.frame(plot_df_dist_isthmus, plot_df_dist_isthmus_std)
plot_dist_df$stand <- factor(plot_dist_df$stand, levels = c("Unstandardised", "Standardised"))

plot_all_start_dists <- plot_dist_df %>% 
  ggplot(aes(x = start, y = dist_to_isthmus)) +
  geom_violin(adjust = .75, scale = "width", linewidth = 0.1, alpha = 0.5, aes(fill = factor(start))) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.1, linewidth = 0.2) +
  geom_point(aes(y = mean), size = 0.5) +
  scale_fill_manual(values = c("#fcbba1", "#ccece6")) +
  labs(x = NULL, y = NULL) +
  facet_grid(stand~model, scale = "free_y") +
  labs(x = "Ancestral area", y = "Distance to isthmus (km)") +
  theme(axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 5),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        legend.position = "none",
        panel.background = element_rect(fill = "#eee4f4ff", colour = "black"),
        panel.grid.major = element_line(linewidth = 0.1, color = "grey50"),
        panel.grid.minor = element_line(linewidth = 0.1, color = "grey50"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        strip.background = element_rect(fill = "#DDE6F5"))


ggsave("./Figures/MS/Supp/metrics_std/panel_all_starting_distances.pdf", 
       plot = plot_all_start_dists, height = 130, width = 200, units = "mm")

ggsave("./Figures/MS/Supp/metrics_std/panel_all_starting_distances.png", 
       plot = plot_all_start_dists, height = 130, width = 200, dpi = 600, units = "mm")
