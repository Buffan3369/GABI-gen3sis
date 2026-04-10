################################################################################
# Name: Climate_maps_snappshots_Oscillayers.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Plot snapshots of BIO1, BIO12 extracted from the Oscillayers dataset
################################################################################

library(tidyverse)
library(scico)
library(ggpubr)
library(hash)

# Temperature snapshots
Temp <- readRDS("./Data/Oscillayers/upscaled_temperature_snapshots_Americas.RDS")
# Precipitation snapshots
Prec <- readRDS("./Data/Oscillayers/upscaled_precipitation_snapshots_Americas.RDS")

int_names <- hash("5 Ma" = 6, "3 Ma" = 5, "2.58 Ma" = 4, "1 Ma" = 3, 
                  "Last Interglacial" = 2, "Last Glacial Maximum" = 1)

Temp_DF <- data.frame(lon = NA, lat = NA, Temp = NA, t = NA)
Prec_DF <- data.frame(lon = NA, lat = NA, Prec = NA, t = NA)

for(i in keys(int_names)){
  yr <- values(int_names[i])
  ## Subset temperature estimates for the Americas
  r_Temp <- Temp[[yr]]
  xy_Temp <- xyFromCell(r_Temp, cell = 1:ncell(r_Temp))
  xyz_Temp <- cbind(xy_Temp, r_Temp@data@values) %>% as.data.frame()
  colnames(xyz_Temp) <- c("lon", "lat", "Temp")
  xyz_Temp$t <- i
  Temp_DF <- rbind(Temp_DF, xyz_Temp %>% mutate(Temp = Temp/10))
  ## Same for precipitations
  r_Prec <- Prec[[yr]]
  xy_Prec <- xyFromCell(r_Prec, cell = 1:ncell(r_Prec))
  xyz_Prec <- cbind(xy_Prec, r_Prec@data@values) %>% as.data.frame()
  colnames(xyz_Prec) <- c("lon", "lat", "Prec")
  xyz_Prec$t <- i
  Prec_DF <- rbind(Prec_DF, xyz_Prec)
}

# Temperature plot
Temp_plot <- Temp_DF %>% 
  filter(!is.na(Temp)) %>% 
  mutate(t = factor(t, levels = c("5 Ma", "3 Ma", "2.58 Ma", "1 Ma","Last Interglacial", "Last Glacial Maximum"))) %>% 
  ggplot(aes(x = lon, y = lat, fill = Temp)) +
  geom_tile() +
  scale_fill_scico(palette = "vik", midpoint = median(Temp_DF$Temp, na.rm = T)) +
  labs(x = NULL, y = NULL, fill = "Temperature \n(°C)") +
  facet_grid(.~t) +
  theme(panel.background = element_rect(fill = "grey60",
                                        colour = "black",
                                        linewidth = 0.1),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        strip.background = element_rect(fill = "bisque2"),
        legend.title = element_text(size = 8, hjust = 0.5),
        legend.text = element_text(size = 6))

# Precipitations plot
Prec_plot <- Prec_DF %>% 
  filter(!is.na(Prec)) %>% 
  mutate(t = factor(t, levels = c("5 Ma", "3 Ma", "2.58 Ma", "1 Ma","Last Interglacial", "Last Glacial Maximum"))) %>% 
  ggplot(aes(x = lon, y = lat, fill = Prec)) +
  geom_tile() +
  scale_fill_scico(palette = "bamO") +
  labs(x = NULL, y = NULL, fill = "Precipitation \n(mm/yr)") +
  facet_grid(.~t) +
  theme(panel.background = element_rect(fill = "grey60",
                                        colour = "black",
                                        linewidth = 0.1),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.title = element_text(size = 8, hjust = 0.5),
        legend.text = element_text(size = 6))

# Assemble and save
comb_oscill <- ggarrange(Temp_plot, Prec_plot, nrow = 2, heights = c(1.1, 1))
ggsave("./Figures/MS/Supp/Clim_snapshots/Climatic_snapshots_panel_Oscillayers.pdf", plot = comb_oscill, 
       height = 120, width = 260, units = "mm")
