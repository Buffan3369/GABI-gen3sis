################################################################################
# Name: Figure_metrics.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Display the three postprocessing metrics:
#         - Proportion of colonised area
#         - Colonised alpha-diversity
#         - Distance to isthmus
################################################################################

library(tidyverse)
source("./R/PostProcessing/4-Extract_distribs_with_moments.R")

## Load plot dataframes for the three metrics ----------------------------------
# Proportion of colonised area
plot_df_prop_col_area <- summarise_distrib(metric = "prop_col_area",
                                           what = "exchanged")
plot_df_prop_col_area <- plot_df_prop_col_area %>% 
  mutate(metric = "Proportion of colonised area") %>% 
  rename(value = "prop_col_area")
# Colonised alpha-diversity
plot_df_div_col_area <- summarise_distrib(metric = "div_col",
                                          what = "exchanged")
plot_df_div_col_area <- plot_df_div_col_area %>% 
  # Log-transform the data for plotting
  mutate(metric = "Log(Colonised \u03b1 diversity)") %>% 
  mutate(div_col = log(div_col, base = 10),
         mean = log(mean, base = 10),
         lower_ci = log(lower_ci, base = 10),
         upper_ci = log(upper_ci, base = 10)) %>% 
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
                                                    "Log(Colonised \u03b1 diversity)",
                                                    "Distance to isthmus (km)"))

full_panel <- PLOT_DF %>% 
  mutate(start = sapply(X = start,
                        FUN = function(x){
                          paste0(x, " America")
                        })) %>% 
  ggplot(aes(x = start, y = value)) +
  geom_violin(adjust = .75, scale = "width", linewidth = 0.1, alpha = 0.5, aes(fill = factor(start))) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.1, linewidth = 0.2) +
  geom_point(aes(y = mean), size = 0.5) +
  scale_fill_manual(values = c("#fcbba1", "#ccece6")) +
  labs(x = NULL, y = NULL) +
  facet_grid(metric~model, scale = "free_y") +
  labs(x = "Ancestral continent", y = NULL) +
  theme(axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 5),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        legend.position = "none",
        panel.background = element_rect(fill = "#F7F2E0", colour = "black"),
        panel.grid.major = element_line(linewidth = 0.1, color = "grey50"),
        panel.grid.minor = element_line(linewidth = 0.1, color = "grey50"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        strip.background = element_rect(fill = "#DDE6F5"))

ggsave("./Figures/MS/Main/Figure_metrics/Figure_metrics_baseline.pdf", 
       plot = full_panel, height = 200, width = 200, units = "mm")




