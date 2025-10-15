################################################################################
# Title: Figure_biomes.R
# Author: Lucas Buffan (lucas.l.buffan@gmail.com)
# Goal: Mapping biome at simulations t_start and proportion of realised exchange
#       per biome.
################################################################################

library(raster)
library(sf)
library(tidyverse)

source("./R/useful/helper_functions.R")

################################################################################
###################### 1. BROADCLASS BIOME MAP AT t=5Ma ########################
################################################################################

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

ggsave("./Figures/MS/Main/Figure_biomes/biome_map.pdf", plot = biome_5M_plot, height = 10, width = 8)


################################################################################
################ 2. PROPORTION OF REALISED EXCHANGE PER BIOME ##################
################################################################################

# Nb of starting species per biome
per_biome_df <- P_tbl %>% 
  filter(!(is.na(start_biome))) %>% 
  count(start_biome, model, start_region)
# Same but only for exchanged species
per_biome_exch_df <- P_tbl %>% 
  filter(!(is.na(start_biome)) & exchanged == 1) %>% 
  count(start_biome, model, start_region)
## No polar species exchanged from NA in M0, no cold species exchanged from SA in M0: add them
per_biome_exch_df <- per_biome_exch_df %>% 
  add_row(start_biome = "5", model = "M0", start_region = "North America", n = 0, .before = 32) %>% 
  add_row(start_biome = "4", model = "M0", start_region = "South America", n = 0, .before = 26)
# Assess proportion
per_biome_exch_df <- per_biome_exch_df %>% 
  mutate(n_init = per_biome_df$n) %>% 
  mutate(prop_exch = n / per_biome_df$n)
# Plot
prop_biome_success_plot <- per_biome_exch_df %>% 
  ggplot(aes(x = start_biome, y = prop_exch)) +
  geom_col(aes(fill = start_biome), linewidth = 0.3, colour = "black") +
  geom_text(aes(label = sapply(prop_exch, round, digits = 2)), inherit.aes = TRUE, nudge_y = 0.05, size = 2.5) +
  scale_fill_manual(values = c("1" = "#f9d14a", "2" = "#ab3329", "3" = "#ed968c", "4" = "#7c4b73", "5" = "#88a0dc")) +
  scale_x_discrete(labels = c("1" = "Tropical", "2" = "Arid", "3" = "Temperate", "4" = "Cold", "5" = "Polar")) +
  labs(x = "Ancestral biome", y = "Prop. success") +
  facet_grid(model~start_region) +
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 10),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        legend.position = "none",
        strip.background = element_rect(fill = "#DDE6F5"),
        strip.text = element_text(size = 10),
        panel.background = element_rect(fill = "grey85"),
        panel.grid.major = element_line(linewidth = 0.25),
        panel.grid.minor = element_line(linewidth = 0.25),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

# ggsave(paste0("./Figures/starting_biome/prop_biome_success.pdf"), 
#        plot = prop_biome_success_plot, height = 170, width = 170, units = "mm")
# 
# ggsave(paste0("./Figures/starting_biome/prop_biome_success.png"), 
#        plot = prop_biome_success_plot, height = 170, width = 170, dpi = 600, units = "mm")

ggsave(paste0("./Figures/MS/Main/Figure_biomes/prop_biome_success.pdf"), 
       plot = prop_biome_success_plot, height = 170, width = 170, units = "mm")
