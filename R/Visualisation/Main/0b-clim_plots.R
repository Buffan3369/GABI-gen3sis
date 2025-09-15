################################################################################
# Name: .R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Plot average climate estimates across space for North and South America 
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
av_clim %>% 
  # Switch time to Myrs
  mutate(time = time / 1000) %>% 
  ggplot(aes(x = time, y = clim_av)) +
  annotate(geom = "rect", xmin = 3.45, xmax = 3.55, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#810f7c") +
  geom_line(lwd = 0.7) +
#  scale_colour_manual(values = c("#fc9272", "#99d8c9")) +
  scale_x_reverse() +
  labs(x = "Time (Ma)", y = NULL) +
  theme_lucas(legend.position = "none") +
  facet_wrap(continent~var, scales = "free", labeller = labeller(continent = striplabs_cont,
                                                                 var = striplabs_clim)) +
  coord_geo(dat = "epochs", abbr = F, center_end_labels = T)
