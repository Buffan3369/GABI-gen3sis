################################################################################
# Title: Biome_map.R
# Author: Lucas Buffan (lucas.l.buffan@gmail.com)
# Goal: Map broad class biomes at t=5Ma
################################################################################

library(raster)
library(sf)
library(tidyverse)

source("./R/useful/helper_functions.R")

sf_use_s2(FALSE)

NthAm <- sf::read_sf("./Data/Shapefile_masks/clean_representations/Nth_Am.shp")
SthAm <- sf::read_sf("./Data/Shapefile_masks/clean_representations/Sth_Am.shp")
Americas <- st_union(NthAm, SthAm) 

biome_5M <- readRDS("./Data/PALEO_PGEM-bioclim/KG_biomes/KG_5Ma_broadclass.RDS")
biome_5M_df <- as.data.frame(biome_5M, xy = TRUE)
colnames(biome_5M_df) <- c("lon", "lat", "biome")
biome_5M_df <- biome_5M_df %>%
  filter(!(is.na(biome))) %>% 
  mutate(biome = sapply(biome, FUN = biome_rename)) %>% 
  mutate(biome = as.factor(biome))

biome_5M_plot <- Americas %>% ggplot() +
  geom_tile(data = biome_5M_df, aes(x = lon, y = lat, fill = biome)) +
  scale_fill_manual(values = c("Tropical" = "#f9d14a",
                               "Arid" = "#ab3329",
                               "Temperate" = "#ed968c", 
                               "Cold" = "#7c4b73", 
                               "Polar" = "#88a0dc")) +
  geom_sf(alpha = 0.01, lwd = 0.5) +
  labs(x = "Longitude", y = "Latitude", fill = "Biome", colour = NULL) +
  ggtitle("5 Ma") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        panel.grid.major = element_line(linetype = 3, colour = "black", linewidth = 0.1),
        panel.grid.minor = element_line(linetype = 3, colour = "black", linewidth = 0.1),
        panel.background = element_rect(fill = "white"),
        plot.margin=grid::unit(c(0,0,0,0), "mm"))

ggsave("./Figures/MS/Main/Figure3/biome_map.pdf", plot = biome_5M_plot, height = 10, width = 8)

