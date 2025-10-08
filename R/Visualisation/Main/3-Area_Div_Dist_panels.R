################################################################################
# Name: 3-Area_Div_Dist_panels.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Display postprocessing metrics
################################################################################

library(tidyverse)

################################################################################
#################### 1. PROPORTION OF SUCCESSFUL EXCHANGES #####################
################################################################################

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



################################################################################
###################### 2. PROPORTION OF COLONISED AREA #########################
################################################################################

plot_df_prop_col_area <- readRDS("./Results/Exchanged_metrics/Prop_colonised_area.RDS")

area_plot2 <- plot_df_prop_col_area %>% 
  ggplot(aes(x = Ori, y = Prop_col_area)) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.15) +
  geom_point(aes(colour = Ori), size = 1.5) +
  scale_colour_manual(values = c("#fb6a4aff", "#66c2a4ff")) +
  labs(x = NULL, y = NULL) +
  facet_grid(.~Model) +
  labs(x = "Ancestral area", y = "Proportion of colonised area") +
  theme(axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 5),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        legend.position = "none",
        panel.background = element_rect(fill = "#F7F2E0", colour = "black"),
        panel.grid.major = element_line(linewidth = 0.1, color = "grey50"),
        panel.grid.minor = element_line(linewidth = 0.1, color = "grey50"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        strip.background = element_rect(fill = "#DDE6F5"))

ggsave("./Figures/prop_col_area/prop_col_area_panel_CI.pdf", 
       plot = area_plot2, height = 80, width = 170, units = "mm")

ggsave("./Figures/prop_col_area/prop_col_area_panel_CI.png", 
       plot = area_plot2, height = 80, width = 170, dpi = 600, units = "mm")



################################################################################
##################### 3. DIVERSITY IN THE COLONISED AREA #######################
################################################################################

plot_df_div_col_area <- readRDS("./Results/Exchanged_metrics/Div_col_area.RDS")

div_plot2 <- plot_df_div_col_area %>% 
  ggplot(aes(x = Ori, y = Div_col_area)) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.15) +
  geom_point(aes(colour = Ori), size = 1.5) +
  scale_colour_manual(values = c("#fb6a4aff", "#66c2a4ff")) +
  labs(x = NULL, y = NULL) +
  facet_grid(.~Model) +
  labs(x = "Ancestral area", y = "Diversty in the colonised area") +
  theme(axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 5),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        legend.position = "none",
        panel.background = element_rect(fill = "#EAF4FB", colour = "black"),
        panel.grid.major = element_line(linewidth = 0.1, color = "grey50"),
        panel.grid.minor = element_line(linewidth = 0.1, color = "grey50"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        strip.background = element_rect(fill = "#DDE6F5"))

ggsave("./Figures/div_col_area/Panel_point_CI_diversity.pdf", 
       plot = div_plot2, height = 80, width = 170, units = "mm")

ggsave("./Figures/div_col_area/Panel_point_CI_diversity.png", 
       plot = div_plot2, height = 80, width = 170, dpi = 600, units = "mm")


################################################################################
######################### 4. DISTANCE TO THE ISTHMUS ###########################
################################################################################

plot_df_dist_isthmus <- readRDS("./Results/Exchanged_metrics/Dist_isthmus.RDS")

dist_plot2 <- plot_df_dist_isthmus %>% 
  ggplot(aes(x = Ori, y = Dist)) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.15) +
  geom_point(aes(colour = Ori), size = 1.5) +
  scale_colour_manual(values = c("#fb6a4aff", "#66c2a4ff")) +
  labs(x = NULL, y = NULL) +
  facet_grid(.~Model) +
  labs(x = "Ancestral area", y = "Distance to the isthmus (km)") +
  theme(axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 5),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        legend.position = "none",
        panel.background = element_rect(fill = "#F5F0FF", colour = "black"),
        panel.grid.major = element_line(linewidth = 0.1, color = "grey50"),
        panel.grid.minor = element_line(linewidth = 0.1, color = "grey50"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        strip.background = element_rect(fill = "#DDE6F5"))

ggsave("./Figures/dist_to_isthm/distance_panel_point_CI.pdf", 
       plot = dist_plot2, height = 80, width = 170, units = "mm")

ggsave("./Figures/dist_to_isthm/distance_panel_point_CI.png", 
       plot = dist_plot2, height = 80, width = 170, dpi = 600, units = "mm")