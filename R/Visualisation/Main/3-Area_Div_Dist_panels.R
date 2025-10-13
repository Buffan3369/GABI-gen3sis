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

plot_df_prop_success <- readRDS("./Results/Exchanged_metrics/Prop_successful_exchange.RDS")

prop_plot2 <- plot_df_prop_success %>% 
  ggplot(aes(x = Ori, y = Prop_success)) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.15) +
  geom_point(aes(colour = Ori), size = 1) +
  scale_colour_manual(values = c("#fb6a4aff", "#66c2a4ff")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.12), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  labs(x = NULL, y = NULL) +
  facet_wrap(.~Model, ncol = 2) +
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

# ggsave("./Figures/prop_successful_exch/Panel_point_CI.pdf", 
#        plot = prop_plot2, height = 80, width = 170, units = "mm")
# 
# ggsave("./Figures/prop_successful_exch/Panel_point_CI.png", 
#        plot = prop_plot2, height = 80, width = 170, dpi = 600, units = "mm")

ggsave("./Figures/MS/Main/Figure2/Figure2.png", 
       plot = prop_plot2, height = 120, width = 110, dpi = 600, units = "mm")


################################################################################
############ 2. LARGE PANEL SUMMARISING THE 3 REMAINING METRICS ################
################################################################################

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

ggsave("./Figures/MS/Main/Figure2/Figure3_baseline.pdf", 
       plot = full_panel, height = 200, width = 200, units = "mm")

ggsave("./Figures/MS/Main/Figure2/Figure3_baseline.png", 
       plot = full_panel, height = 200, width = 200, dpi = 600, units = "mm")



