library(tidyverse)
library(scico)
library(ggpubr)
library(raster)
library(hash)

# Mask of the Americas
Americas <- shapefile("./Data/Shapefile_masks/raw_mask.shp")
# Temperature
Temp <- read.table("./Data/PALEO_PGEM-bioclim/bio1_mean.txt", header = T)
Temp_sub <- Temp %>% dplyr::select(Long, Lat, T_5000, T_3000, T_2580, T_1000, T_120, T_21, T_0)
rm(Temp)
# Precipitations
Prec <- read.table("./Data/PALEO_PGEM-bioclim/bio12_mean.txt", header = T)
Prec_sub <- Prec %>% dplyr::select(Long, Lat, T_5000, T_3000, T_2580, T_1000, T_120, T_21, T_0)
rm(Prec)
# KG biomes
biomes <- readRDS("./Data/PALEO_PGEM-bioclim/KG_biomes/KG_biome_maps.RDS")
# Function to turn fine- to broadclasses
broad <- function(class_vect){
  # Tropical
  class_vect[which(class_vect %in% c(1,2,3))] <- "Tropical"
  # Arid
  class_vect[which(class_vect %in% c(4,5,6))] <- "Arid"
  # Temperate
  class_vect[which(class_vect %in% 7:15)] <- "Temperate"
  # Cold
  class_vect[which(class_vect %in% 16:27)] <- "Cold"
  # Polar
  class_vect[which(class_vect %in% c(28,29,30))] <- "Polar"
  return(class_vect)
}

int_names <- hash("5 Ma" = 5000, "3 Ma" = 3000, "2.58 Ma" = 2580, "1 Ma" = 1000, 
                  "Last Interglacial" = 120, "Last Glacial Maximum" = 21, "Present" = 0)

Temp_DF <- data.frame(lon = NA, lat = NA, Temp = NA, t = NA)
Prec_DF <- data.frame(lon = NA, lat = NA, Prec = NA, t = NA)
Biome_DF <- data.frame(lon = NA, lat = NA, Biome = NA, t = NA)

for(i in keys(int_names)){
  yr <- values(int_names[i])
  # Subset temperature estimates for the Americas
  tmp_Temp <- Temp_sub[, c("Long", "Lat", paste0("T_", yr))]
  r_Temp <- rasterFromXYZ(tmp_Temp)
  r_Temp_Am <- raster::extract(r_Temp, Americas, cellnumbers = TRUE, df = TRUE)
  xy_extracted_Temp <- coordinates(r_Temp)[r_Temp_Am$cell,] %>% as.data.frame()
  xyz_extracted_Temp <- data.frame(cbind(xy_extracted_Temp, r_Temp_Am[,3]))
  colnames(xyz_extracted_Temp) <- c("lon", "lat", "Temp")
  xyz_extracted_Temp$t <- i
  Temp_DF <- rbind(Temp_DF, xyz_extracted_Temp)
  # Same for precipitations
  tmp_Prec <- Prec_sub[, c("Long", "Lat", paste0("T_", yr))]
  r_Prec <- rasterFromXYZ(tmp_Prec)
  r_Prec_Am <- raster::extract(r_Prec, Americas, cellnumbers = TRUE, df = TRUE)
  xy_extracted_Prec <- coordinates(r_Prec)[r_Prec_Am$cell,] %>% as.data.frame()
  xyz_extracted_Prec <- data.frame(cbind(xy_extracted_Prec, r_Prec_Am[,3]))
  colnames(xyz_extracted_Prec) <- c("lon", "lat", "Prec")
  xyz_extracted_Prec$t <- i
  Prec_DF <- rbind(Prec_DF, xyz_extracted_Prec)
  # Subset corresponding biome
  corr_biome <- biomes[[(yr+1)]]
  corr_biome@data@values <- broad(corr_biome@data@values) # Switch to broad classes
  corr_biome_df <- xyFromCell(corr_biome, cell = 1:ncell(corr_biome))
  corr_biome_df <- corr_biome_df %>% 
    as.data.frame() %>% 
    rename(lon = "x", lat = "y") %>% 
    mutate(Biome = corr_biome@data@values, t = i)
  Biome_DF <- rbind(Biome_DF, corr_biome_df)
}

# Temperature plot
Temp_plot <- Temp_DF %>% 
  filter(!is.na(Temp)) %>% 
  mutate(t = factor(t, levels = c("5 Ma", "3 Ma", "2.58 Ma", "1 Ma","Last Interglacial", "Last Glacial Maximum", "Present"))) %>% 
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
  mutate(t = factor(t, levels = c("5 Ma", "3 Ma", "2.58 Ma", "1 Ma","Last Interglacial", "Last Glacial Maximum", "Present"))) %>% 
  ggplot(aes(x = lon, y = lat, fill = Prec)) +
  geom_tile() +
  scale_fill_scico(palette = "bamO") +
  labs(x = NULL, y = NULL, fill = "Precipitations \n(mm/yr)") +
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

# Biome plot
Biome_plot <- Biome_DF %>% 
  filter(!is.na(Biome)) %>% 
  mutate(t = factor(t, levels = c("5 Ma", "3 Ma", "2.58 Ma", "1 Ma","Last Interglacial", "Last Glacial Maximum", "Present")),
         Biome = factor(Biome, levels = c("Tropical", "Arid", "Temperate", "Cold", "Polar"))) %>% 
  ggplot(aes(x = lon, y = lat, fill = Biome)) +
  geom_tile() +
  scale_fill_manual(values = c("#f9d14a","#ab3329", "#ed968c", "#7c4b73", "#88a0dc")) +
  labs(x = NULL, y = NULL, fill = "Biome") +
  facet_grid(.~t) +
  theme(panel.background = element_rect(fill = "transparent",
                                        colour = "black",
                                        linewidth = 0.1),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        strip.background = element_rect(fill = "bisque2"),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6))
# Assemble and save
comb <- ggarrange(Temp_plot, Prec_plot, Biome_plot, nrow = 3)
ggsave("./Figures/MS/Supp/Climatic_snapshots_panel.pdf", plot = comb, height = 150,
       width = 300, units = "mm")
  