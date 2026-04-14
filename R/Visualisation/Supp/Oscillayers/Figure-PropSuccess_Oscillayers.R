################################################################################
# Name: Figure_PropSuccess_Oscillayers.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Plot proportion of successful exchanges across starting continents and
#       models for Oscillayers-based simulations.
################################################################################

library(tidyverse)

plot_df_prop_success <- readRDS("./Results/Exchanged_metrics/Prop_successful_exchange.RDS")

prop_plot2 <- plot_df_prop_success %>% 
  ggplot(aes(x = Ori, y = Prop_success)) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.15) +
  geom_point(aes(fill = Ori), colour = "black", pch = 23, size = 1.5) +
  scale_fill_manual(values = c("#fb6a4aff", "#66c2a4ff")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.12), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  labs(x = NULL, y = NULL) +
  facet_wrap(.~Model, ncol = 2) +
  labs(x = "Ancestral continent", y = "Proportion of successful exchange") +
  theme(axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 5),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major = element_line(linewidth = 0.1, color = "grey50"),
        panel.grid.minor = element_line(linewidth = 0.1, color = "grey50"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        strip.background = element_rect(fill = "#DDE6F5"))

ggsave("./Figures/MS/Main/Figure_PropSuccess/Figure_PropSuccess.png", 
       plot = prop_plot2, height = 120, width = 110, dpi = 600, units = "mm")
