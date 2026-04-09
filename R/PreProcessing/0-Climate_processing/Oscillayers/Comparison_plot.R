library(raster)
library(scico)
library(tidyverse)

r1 <- raster("~/Téléchargements/doi_10_5061_dryad_27f8s90__v20190725/Bio1_Pleistocene/bio1_t10.asc")

# Original resolution
americas <- shapefile("./Data/Shapefile_masks/raw_mask.shp")
oscill_extr <- raster::extract(r1, americas, cellnumbers = T, df = T)
xy <- coordinates(r1)[oscill_extr$cell,] %>% as.data.frame()
xyz_original <- cbind(xy, oscill_extr[,3])
colnames(xyz_original) <- c("lon", "lat", "temp")
xyz_original$resolution <- "Original resolution"

# Upscaled resolution
xy_ref <- readRDS("./Data/PALEO_PGEM-bioclim/Cell_Coordinates_Americas.RDS")
oscill_am_extracted <- raster::extract(r1, xy_ref, cellnumbers = T, df = T)
xyz_upscaled <- cbind(xy_ref, oscill_am_extracted[,3])
colnames(xyz_upscaled) <- c("lon", "lat", "temp")
xyz_upscaled$resolution <- "Upscaled resolution"

# Comparison plot
merged <- rbind(xyz_original, xyz_upscaled)
merged_plt <- merged %>% 
  filter(!is.na(temp)) %>% 
  mutate(temp = temp/10) %>% 
  ggplot(aes(x = lon, y = lat, fill = temp)) +
  scale_fill_scico(palette = "vik") +
  geom_tile() +
  facet_grid(.~resolution) +
  labs(x = NULL, y = NULL, fill = "Temperature \n(°C)") +
  theme(panel.background = element_rect(fill = "grey60",
                                        colour = "black",
                                        linewidth = 0.1),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        strip.background = element_rect(fill = "bisque2"),
        strip.text = 
        legend.title = element_text(size = 8, hjust = 0.5),
        legend.text = element_text(size = 6))
ggsave("./Figures/MS/Supp/Oscillayers/Upscaling_illustration.png", plot = merged_plt,
       dpi = 600, height = 80, width = 150, units = "mm")
