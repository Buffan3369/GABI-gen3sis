################################################################################
# Name: Figure_framework.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Plot average climate estimates across space for North and South America
#       and continental landmasses.
################################################################################

library(tidyverse)
library(deeptime)

source("./R/useful/helper_functions.R")

av_clim <- readRDS("./Data/PALEO_PGEM-bioclim/average_clim_both_continents.RDS")

striplabs_cont <- c("North America", "South America")
names(striplabs_cont) <- c("North", "South")

striplabs_clim <- c("Precipitations (mm)", "Temperature (°C)")
names(striplabs_clim) <- c("Precipitations", "Temperature")

## Temperature -----------------------------------------------------------------
clim_plot <- av_clim %>% 
  # Switch time to Myrs
  mutate(time = time / 1000) %>% 
  ggplot(aes(x = time, y = clim_av)) +
  annotate(geom = "rect", xmin = 3.45, xmax = 3.55, ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "#810f7c") +
  geom_line(lwd = 0.15) +
#  scale_colour_manual(values = c("#fc9272", "#99d8c9")) +
  scale_x_reverse() +
  labs(x = "Time (Ma)", y = NULL) +
  theme_lucas(legend.position = "none") +
  facet_wrap(continent~var, scales = "free", labeller = labeller(continent = striplabs_cont,
                                                                 var = striplabs_clim)) +
  coord_geo(dat = "epochs", abbr = F, center_end_labels = T, height = unit(0.7, "line"), size = 2.5)

ggsave(filename = "./Figures/MS/Main/Figure_framework/clim_panel.pdf", plot = clim_plot, height = 10, width = 13, unit = "cm")


## Continental masses ----------------------------------------------------------
NthA <- sf::st_read("./Data/Shapefile_masks/clean_representations/Nth_Am.shp")
NA_sil <- NthA %>% ggplot() +
  geom_sf(fill = "#fb6a4a", linewidth = 0.1) +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave("./Figures/MS/Main/Figure_framework/North_America.pdf", plot = NA_sil, height = 75, width = 105, units = "mm")
# South America
SthA <- sf::st_read("./Data/Shapefile_masks/clean_representations/Sth_Am.shp")
SA_sil <- SthA %>% ggplot() +
  geom_sf(fill = "#66c2a4", linewidth = 0.1) +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave("./Figures/MS/Main/Figure_framework/South_America.pdf", plot = SA_sil, height = 75, width = 105, units = "mm")
