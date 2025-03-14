################################################################################
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Aim: Some illustrative figures
################################################################################

library(raster)
library(terra)
library(h3jsr)
library(tidyverse)

## Equal-area plots ------------------------------------------------------------
landscape <- readRDS("./Results/M0/try_north/landscapes/landscape_t_4683.rds")

eq_area_plot <- function(landscape, region){
  # Region coordinates & initial environment
  if(region == "North"){
    Region_coordinates <- landscape$coordinates[which(landscape$coordinates[,1] < -54 & 
                                                        landscape$coordinates[,2] > 12.5),] 
    fill_c <- "#fb6a4a"
    psz <- 0.1
  }
  else if(region == "South"){
    Region_coordinates <- landscape$coordinates[which(landscape$coordinates[,1] < -33 & 
                                                               landscape$coordinates[,2] < 6),]
    fill_c <- "#66c2a4"
    psz <- 0.3
  }
  Region_environment <- rep(1, nrow(Region_coordinates))
  # Rasterise
  Region_raster <- raster::rasterFromXYZ(cbind(Region_coordinates, Region_environment))
  # Extend initial raster to reach the extent of the landscape
  Region_raster <- raster::extend(Region_raster, landscape$extent)
  
  ## 2. Generate equal-area grid of similar resolution as our raster -----------
  # Create a "bullshit" `terra` SpatRaster with the same extent but uninformative layer
  bullshit_env <- Region_environment
  Region_terra <- raster::rasterFromXYZ(cbind(Region_coordinates, bullshit_env))
  Region_terra <- raster::extend(Region_terra, landscape$extent)
  Region_terra <- as(Region_terra, "SpatRaster")
  # Equal-area grid
  cells <- h3jsr::get_res0()
  cells <- h3jsr::get_children(h3_address = cells, res = 3) # res = 3 corresponds to cell area of ~12393km², ~~1x1° equator
  cells <- unlist(cells)
  # Get centroids of cells
  xy <- h3jsr::cell_to_point(cells, simple = FALSE)
  # Extract coordinates
  xy <- sf::st_coordinates(xy)
  xy <- data.frame(xy)
  # Update column names
  colnames(xy) <- c("lng", "lat")
  
  ## 3. Extract centroids contained in our desired ancestral area --------------
  # Convert raster to polygon
  r_poly <- terra::as.polygons(Region_terra) |> sf::st_as_sf(Region_terra)
  # Convert centroids to sf points
  xy_sf <- sf::st_as_sf(xy, coords = c("lng", "lat"), crs = sf::st_crs(r_poly))
  # Filter points inside the raster extent
  xy_inside <- xy_sf[sf::st_intersects(xy_sf, r_poly, sparse = FALSE), ]
  # Convert back to dataframe
  xy_filtered <- sf::st_coordinates(xy_inside) |> as.data.frame()
  colnames(xy_filtered) <- c("lng", "lat")
  
  ## 4. Plot -------------------------------------------------------------------
  plot.df <- as.data.frame(Region_raster, xy = TRUE)
  plot.df <- plot.df %>% filter(!(is.na(Region_environment)))
  eq_area_plot <- ggplot() +
    geom_tile(data = plot.df, aes(x = x, y = y, fill = Region_environment)) +
    scale_fill_continuous(low = fill_c, high = fill_c) +
    geom_point(data = xy_filtered, aes(x = lng, y = lat), size = psz) +
    theme(legend.position = "none",
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          panel.grid.minor = element_blank())
  return(eq_area_plot)
}

NA_eq_area <- eq_area_plot(landscape, region = "North")
ggsave("../Presentations/GABI/NA_equal_area.pdf", plot = NA_eq_area, height = 100, width = 100, units = "mm")

SA_eq_area <- eq_area_plot(landscape, region = "South")
ggsave("../Presentations/GABI/SA_equal_area.pdf", plot = SA_eq_area, height = 150, width = 100, units = "mm")

## Climate processing ----------------------------------------------------------
  # Global plots
MAT <- read.table("./Data/PALEO_PGEM-bioclim/bio1_mean.txt", header = T)
temp_1k <- MAT[,c(1,2,4000)]
t_plot <- temp_1k %>% ggplot(aes(x = Long, y = Lat, fill = T_1003)) +
  geom_tile() +
  scale_fill_distiller(palette = "RdYlBu") +
  labs(fill = "Temperature (°C)")
rm(MAT)
ggsave("../Presentations/GABI/p_clim/temp_1003.pdf", plot = t_plot, height = 100, width = 200, units = "mm")

MAP <- read.table("./Data/PALEO_PGEM-bioclim/bio12_mean.txt", header = T)
prec_1k <- MAP[,c(1,2,4000)]
p_plot <- prec_1k %>% ggplot(aes(x = Long, y = Lat, fill = T_1003)) +
  geom_tile() +
  scale_fill_distiller(palette = "BrBG", trans = "reverse") +
  labs(fill = "Precipitations (mm/y)")
rm(MAP)
ggsave("../Presentations/GABI/p_clim/prec_1003.pdf", plot = p_plot, height = 100, width = 200, units = "mm")

  # Regionally-subsampled ones
subsample <- function(xyz){
  mask <- shapefile("./Data/Shapefile_masks/raw_mask.shp")
    # Corresponding raster
  r <- rasterFromXYZ(xyz, crs = crs(mask))
    # Extract values contained in the vector mask
  clim_am <- raster::extract(r, mask, cellnumbers = TRUE, df = TRUE)
    # Retrieve coordinates of the corresponding cells
  xy_extracted <- coordinates(r)[clim_am$cell,]
    # Construct xyz file and create a raster out of it
  xyz_extracted <- data.frame(cbind(xy_extracted, clim_am[,3]))
  return(xyz_extracted)
}
    # Prec
prec_1k_am <- subsample(prec_1k)
colnames(prec_1k_am) <- colnames(prec_1k)
p_plot_am <- prec_1k_am %>%
  filter(!is.na(T_1003)) %>% 
  ggplot(aes(x = Long, y = Lat, fill = T_1003)) +
  geom_tile() +
  scale_fill_distiller(palette = "BrBG", trans = "reverse") +
  labs(fill = "Precipitations (mm/y)")
ggsave("../Presentations/GABI/p_clim/prec_1003_am.pdf", plot = p_plot_am, height = 100, width = 150, units = "mm")
    # Temp
temp_1k_am <- subsample(temp_1k)
colnames(temp_1k_am) <- colnames(temp_1k)
t_plot_am <- temp_1k_am %>%
  filter(!is.na(T_1003)) %>% 
  ggplot(aes(x = Long, y = Lat, fill = T_1003)) +
  geom_tile() +
  scale_fill_distiller(palette = "RdYlBu") +
  labs(fill = "Temperature (°C)")
ggsave("../Presentations/GABI/p_clim/temp_1003_am.pdf", plot = t_plot_am, height = 100, width = 150, units = "mm")

## Biome map -------------------------------------------------------------------
kg <- readRDS("./Data/PALEO_PGEM-bioclim/KG_biomes/KG_biome_maps.RDS")
r1k <- kg[[4000]]
df_1k <- data.frame(rasterToPoints(r1k))
colnames(df_1k) <- c("Long", "Lat", "Biome")
plot_biome <- df_1k %>%
  ggplot(aes(x = Long, y = Lat, fill = Biome)) +
  geom_tile() +
  scale_fill_distiller(palette = "Paired")
ggsave("../Presentations/GABI/p_clim/biomes_1000_am.pdf", plot = plot_biome, height = 100, width = 120, units = "mm")

## Isolated continents ---------------------------------------------------------
# North America
NthA <- sf::st_read("./Data/Shapefile_masks/clean_representations/Nth_Am.shp")
NA_sil <- NthA %>% ggplot() +
  geom_sf(fill = "#fb6a4a", linewidth = 0.1) +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave("../Presentations/GABI/North_America.png", plot = NA_sil, dpi = 600, height = 50, width = 70, units = "mm")
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
ggsave("../Presentations/GABI/South_America.png", plot = SA_sil, dpi = 600, height = 50, width = 70, units = "mm")

## Gaussian --------------------------------------------------------------------
x <- seq(-5, 5, 0.001)
y <- sapply(x, FUN = function(x){1/(sqrt(2*pi))*exp((-x**2)/2)})
plt_df <- data.frame(x, y)

p <- plt_df %>%
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  geom_ribbon(aes(ymax = y, ymin = 0), fill = "#00D455") +
  scale_y_continuous(expand = c(0,0), limits = c(0,0.6)) +
  scale_x_continuous(limits = c(-3,6)) +
  theme(axis.line.x = element_line(colour = "black"),
        axis.ticks = element_blank(),
        axis.line.y = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_blank())

ggsave("../Presentations/GABI/gaussian.pdf", plot = p, height = 30, width = 80, units = "mm")

## Sobol' sequences ------------------------------------------------------------
library(randtoolbox)
df_unif <- data.frame(x = runif(n = 600, min = 0, max = 1),
                      y = runif(n = 600, min = 0, max = 1))

df_sobol <- data.frame(sobol(n = 600, dim = 2))

pdf("../Presentations/GABI/param_space_sobol_unif.pdf", height = 7, width = 14)
par(mfrow = c(1,2))
plot(df_unif$x, df_unif$y, main = "Uniform", xlab = "x", ylab = "y")
plot(df_sobol$X1, df_sobol$X2, main = "Sobol'", xlab = "x", ylab = "y")
dev.off()
