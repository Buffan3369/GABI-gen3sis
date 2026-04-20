library(raster)
library(scico)
library(tidyverse)
library(sf)

# Faudra faire un + grand masque...
panama <- shapefile("./Data/Shapefile_masks/Panama.shp")
panama_sf <- st_as_sf(panama)

# temp1 <- read.table("./Data/PALEO_PGEM-bioclim/bio1_mean.txt", header = T)
# temp1 <- temp1[, c(1,2,5003)]
# saveRDS(temp1, "./Data/PALEO_PGEM-bioclim/bio1_xyz_present.RDS")
temp1 <- readRDS("./Data/PALEO_PGEM-bioclim/bio1_xyz_present.RDS")

r1 <- rasterFromXYZ(temp1)
temp1_pan <- raster::crop(r1, panama, cellnumbers = T, df = T)
temp1_pan_xy <- xyFromCell(temp1_pan, cell = 1:ncell(temp1_pan))
temp1_pan_xyz <- cbind(temp1_pan_xy, temp1_pan@data@values) %>% as.data.frame()
colnames(temp1_pan_xyz) <- c("lon", "lat", "temp")

extent_plot <- panama_sf %>% ggplot() +
  geom_sf(fill = "grey50") +
  geom_tile(temp1_pan_xyz, mapping = aes(x = lon, y = lat, fill = temp), alpha = 0.7) +
  scale_fill_scico(palette = "lajolla", na.value="#c6dbef") +
  theme(axis.line = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#c6dbef"),
        legend.position = 'none')
ggsave("./Figures/MS/Supp/Clim_snapshots/Extent_PPGEM.png", dpi = 600,
       plot = extent_plot, height = 70, width = 100, units = "mm")
