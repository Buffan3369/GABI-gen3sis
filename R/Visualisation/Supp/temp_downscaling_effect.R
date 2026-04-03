################################################################################
# Name: temp_downscaling_effect.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Plot average temperature estimates with the original (1ky) and downscaled 
#       (10ky) time steps across space for North and South America
################################################################################

library(tidyverse)
library(deeptime)
library(ggpubr)

source("./R/useful/helper_functions.R")

striplabs_cont <- c("North America", "South America")
names(striplabs_cont) <- c("North", "South")

## Original time step ---------------------------------------------------------- 
av_clim <- readRDS("./Data/PALEO_PGEM-bioclim/average_clim_both_continents.RDS")
# Subset temperature during the Pleistocene
av_clim <- av_clim %>% 
  filter(var == "Temperature") %>% 
  filter(time < 2580)

original_plot <- av_clim %>%
  # Switch time to Myrs
  mutate(time = time / 1000) %>% 
  ggplot(aes(x = time, y = clim_av)) +
  geom_line(lwd = 0.15) +
  scale_x_reverse() +
  labs(x = NULL, y = "Original temperature (°C)") +
  theme_lucas(legend.position = "none") +
  theme(axis.text = element_text(size = 7),
        strip.background = element_rect(fill = "bisque2")) +
  facet_wrap(.~continent, scales = "free", labeller = labeller(continent = striplabs_cont)) 
# +
#   coord_geo(dat = "epochs", abbr = F, center_end_labels = T, height = unit(0.7, "line"), size = 2.5)

## Downscaled time step --------------------------------------------------------
av_clim_down <- av_clim %>% 
  filter(time %in% seq(0, 5000, 10))

downscaled_plot <- av_clim_down %>%
  # Switch time to Myrs
  mutate(time = time / 1000) %>% 
  ggplot(aes(x = time, y = clim_av)) +
  geom_line(lwd = 0.15) +
  scale_x_reverse() +
  labs(x = "Time (Ma)", y = "Downscaled temperature (°C)") +
  theme_lucas(legend.position = "none") +
  theme(axis.text = element_text(size = 7),
        strip.background = element_rect(fill = "bisque2")) +
  facet_wrap(.~continent, scales = "free", labeller = labeller(continent = striplabs_cont))
# +
#   coord_geo(dat = "epochs", abbr = F, center_end_labels = T, height = unit(0.7, "line"), size = 2.5)

## Arrange and save ------------------------------------------------------------
comb_plot <- ggarrange(original_plot, downscaled_plot, ncol = 1, labels = c("(A)", "(B)"), hjust = -0.3)
ggsave("./Figures/MS/Supp/Downscaled_temp.png", comb_plot, dpi = 600, height = 150, width = 200, units = "mm")
