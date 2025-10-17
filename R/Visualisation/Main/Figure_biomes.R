################################################################################
# Title: Figure_biomes.R
# Author: Lucas Buffan (lucas.l.buffan@gmail.com)
# Goal: Mapping biome at simulations t_start and proportion of realised exchange
#       per biome.
################################################################################

library(raster)
library(sf)
library(tidyverse)
library(ggpubr)

source("./R/PostProcessing/4-Extract_distribs_with_moments.R")

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
  mutate(biome = sapply(biome, FUN = biome_rename))

biome_5M_df <- biome_5M_df %>%
  mutate(biome = factor(biome, levels = c("Polar", "Cold", "Temperate", "Arid", "Tropical")))

biome_5M_plot <- Americas %>% ggplot() +
  geom_tile(data = biome_5M_df, aes(x = lon, y = lat, fill = biome)) +
  scale_fill_manual(values = c("Tropical" = "#f9d14a",
                               "Arid" = "#ab3329",
                               "Temperate" = "#ed968c", 
                               "Cold" = "#7c4b73", 
                               "Polar" = "#88a0dc")) +
  geom_sf(alpha = 0.01, lwd = 0.1) +
  labs(x = "Longitude", y = "Latitude", fill = "Biome", colour = NULL) +
  ggtitle("t=5 Ma") +
  theme(axis.text = element_text(size = 4),
        axis.title = element_text(size = 7),
        plot.title = element_text(hjust = 0.5, size = 8),
        legend.key.size = unit(2, 'mm'),
        legend.key.height = unit(2, 'mm'),
        legend.key.width = unit(2, 'mm'),
        legend.key = element_rect(colour = "black"),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 5),
        panel.grid.major = element_line(linetype = 3, colour = "black", linewidth = 0.1),
        panel.grid.minor = element_line(linetype = 3, colour = "black", linewidth = 0.1),
        panel.background = element_rect(fill = "white"),
        plot.margin = unit(c(1, 1, 1, 1), "mm"))


################################################################################
############# 2. NUMBER OF SIMULATION STARTING WITHIN EACH BIOME ###############
################################################################################

P_tbl_all <- summarise_distrib(metric = "start_biome",
                               what = "all")
P_tbl_all <- P_tbl_all %>% 
  mutate(start = sapply(X = start, FUN = function(x){paste0(x, " America")})) %>%
  filter(!is.na(start_biome))

start_biome_all <- P_tbl_all %>% 
  mutate(start_biome = as.character(start_biome)) %>% 
  ggplot(aes(x = start_biome)) +
  geom_bar(aes(fill = start_biome), colour = "black", linewidth = 0.3) +
  geom_text(stat = "count", aes(label=after_stat(count)), size = 1.5, vjust = -0.5) +
  scale_fill_manual(values = c("1" = "#f9d14a", "2" = "#ab3329", "3" = "#ed968c", "4" = "#7c4b73", "5" = "#88a0dc")) +
  labs(x = "Ancestral biome", y = "Number of simulations") +
  scale_x_discrete(labels = c("1" = "Tropical", "2" = "Arid", "3" = "Temperate", "4" = "Cold", "5" = "Polar")) +
  ylim(0, 315) +
  facet_grid(model~start) +
  theme(axis.text = element_text(size = 4),
        axis.title = element_text(size = 7),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        legend.position = "none",
        strip.background = element_rect(fill = "#DDE6F5"),
        strip.text = element_text(size = 7),
        panel.background = element_rect(fill = "grey85"),
        panel.grid.major = element_line(linewidth = 0.25),
        panel.grid.minor = element_line(linewidth = 0.25),
        plot.margin = unit(c(3, 3, 3, 3), "mm"))


################################################################################
################ 3. PROPORTION OF REALISED EXCHANGE PER BIOME ##################
################################################################################

P_tbl_all <- summarise_distrib(metric = "start_biome",
                               what = "all")

P_tbl_exchanged <- summarise_distrib(metric = "start_biome",
                                     what = "exchanged")

# Nb of starting species per biome
per_biome_df <- P_tbl_all %>% 
  filter(!(is.na(start_biome))) %>% 
  mutate(start = sapply(X = start, FUN = function(x){paste0(x, " America")})) %>% 
  count(start_biome, model, start)
# Same but only for exchanged species
per_biome_exch_df <- P_tbl_exchanged %>% 
  mutate(start = sapply(X = start, FUN = function(x){paste0(x, " America")})) %>% 
  count(start_biome, model, start)
## No polar species exchanged from NA in M0, no cold species exchanged from SA in M0: add them
per_biome_exch_df <- per_biome_exch_df %>% 
  add_row(start_biome = 5, model = "M0", start = "North America", n = 0, .before = 32) %>% 
  add_row(start_biome = 4, model = "M0", start = "South America", n = 0, .before = 26)
# Compute proportion
per_biome_exch_df <- per_biome_exch_df %>% 
  mutate(n_init = per_biome_df$n) %>% 
  mutate(prop_exch = n / per_biome_df$n)
# Compute binomial 95% confidence interval
per_biome_exch_df <- per_biome_exch_df %>% 
  mutate(lower_ci = sapply(X = 1:nrow(per_biome_exch_df),
                           FUN = function(i){
                             p <- per_biome_exch_df$prop_exch[i]
                             N <- per_biome_exch_df$n_init[i]
                             lwr <- bino_CI(prop = p, n = N, what = "Lower")
                             if(lwr < 0){
                               lwr <- 0
                             }
                             return(lwr)
                           }),
         upper_ci = sapply(X = 1:nrow(per_biome_exch_df),
                           FUN = function(i){
                             p <- per_biome_exch_df$prop_exch[i]
                             N <- per_biome_exch_df$n_init[i]
                             upr <- bino_CI(prop = p, n = N, what = "Upper")
                             if(upr > 1){
                               upr <- 1
                             }
                             return(upr)
                           }))
# Plot
prop_biome_success_plot <- per_biome_exch_df %>% 
  mutate(start_biome = as.character(start_biome)) %>% 
  ggplot(aes(x = start_biome, y = prop_exch)) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.3) +
  geom_point(aes(fill = start_biome), colour = "black", pch = 23, size = 1.5) +
  scale_fill_manual(values = c("#f9d14a","#ab3329", "#ed968c", "#7c4b73", "#88a0dc")) +
  scale_x_discrete(labels = c("1" = "Tropical", "2" = "Arid", "3" = "Temperate", "4" = "Cold", "5" = "Polar")) +
  scale_y_continuous(limits = c(0, 1.05)) +
  labs(x = "Ancestral biome", y = "Proportion of successful exchange") +
  facet_grid(model~start) +
  theme(axis.text = element_text(size = 5),
        axis.title = element_text(size = 7),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        legend.position = "none",
        strip.background = element_rect(fill = "#DDE6F5"),
        strip.text = element_text(size = 7),
        panel.background = element_rect(fill = "grey90"),
        panel.grid.major = element_line(linewidth = 0.25, colour = "white"),
        panel.grid.minor = element_line(linewidth = 0.25, colour = "white"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        panel.heights = unit(18, "cm"))

################################################################################
############################ 4. ASSEMBLE AND SAVE ##############################
################################################################################

first_col <- ggarrange(biome_5M_plot, start_biome_all, nrow = 2,
                       labels = c("(A)", "(B)"), label.y = c(0.85, 1.05), 
                       heights = c(0.5, 0.5), font.label = list(size = 12))

biome_panel <- ggarrange(first_col, prop_biome_success_plot,
                         ncol = 2, labels = c(NA, "(C)"),
                         widths = c(0.4, 0.6), font.label = list(size = 12))

ggsave("./Figures/MS/Main/Figure_biomes/Figure_biomes.png", plot = biome_panel,
       height = 200, width = 200, dpi = 600, units = "mm")
