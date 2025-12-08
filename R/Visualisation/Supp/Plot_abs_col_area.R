################################################################################
# Name: Plot_abs_col_area.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Display the absolute proportion of colonised area
################################################################################

library(tidyverse)
source("./R/PostProcessing/4-Extract_distribs_with_moments.R")

## Load plot dataframes for the three metrics ----------------------------------
# Proportion of colonised area
plot_df_abs_col_area <- summarise_distrib(metric = "abs_col_area",
                                           what = "exchanged")
plot_df_abs_col_area <- plot_df_abs_col_area %>% 
  mutate(metric = "Absolute colonised area") %>% 
  rename(value = "abs_col_area")

## Merge them ------------------------------------------------------------------
p_abs <- plot_df_abs_col_area %>% 
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
  facet_grid(.~model, scale = "free_y") +
  labs(x = "Ancestral continent", y = "Nb. cells occupied") +
  theme(axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 5),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        legend.position = "none",
        panel.background = element_rect(fill = "#F7F2E0", colour = "black"),
        panel.grid.major = element_line(linewidth = 0.1, color = "grey50"),
        panel.grid.minor = element_line(linewidth = 0.1, color = "grey50"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        strip.background = element_rect(fill = "#DDE6F5"))

ggsave("./Figures/MS/Supp/abs_col_area.png", dpi = 600, 
       plot = p_abs, height = 90, width = 200, units = "mm")




