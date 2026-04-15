################################################################################
# Name: Comparison_temp_variability.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Compute pairwise temperature deviation for each time step, averaged 
#       across space, for Oscillayers and PALEO-PGEM, and plot it.
################################################################################

library(raster)

## Shapefile masks -------------------------------------------------------------
NthAm <- shapefile("./Data/Shapefile_masks/North_America_cut.shp")
SthAm <- shapefile("./Data/Shapefile_masks/South_America_cut.shp")

## Oscillayers -----------------------------------------------------------------
temp_oscill <- readRDS("./Data/Oscillayers/MAT_list_Americas_Oscillayers.RDS")

delta_t_oscill <- c()
delta_t_NA_oscill <- c()
delta_t_SA_oscill <- c()

for(i in 2:length(temp_oscill)){
  r_im1 <- temp_oscill[[i-1]]
  r_i <- temp_oscill[[i]]
  delta_t_oscill <- c(delta_t_oscill,
                      mean(abs(r_im1@data@values/10 - r_i@data@values/10),
                           na.rm = T))
  # North America
  im1_NA <- extract(r_im1, NthAm)[[1]]
  i_NA <- extract(r_i, NthAm)[[1]]
  delta_t_NA_oscill <- c(delta_t_NA_oscill,
                         mean(abs(im1_NA - i_NA)/10, na.rm = T))

  # South America
  im1_SA <- extract(r_im1, SthAm)[[1]]
  i_SA <- extract(r_i, SthAm)[[1]]
  delta_t_SA_oscill <- c(delta_t_SA_oscill,
                         mean(abs(im1_SA - i_SA)/10, na.rm = T))
  if(i %% 10 == 0){
    print(i)
  }
}

## PALEO-PGEM ------------------------------------------------------------------
temp_ppgem <- read.table("./Data/PALEO_PGEM-bioclim/bio1_mean.txt", header = T)
temp_ppgem <- temp_ppgem[, c(1,2, seq(3, 5003, 10))]

delta_t_ppgem <- c()
delta_t_NA_ppgem <- c()
delta_t_SA_ppgem <- c()

for(i in 5:(ncol(temp_ppgem)-1)){ # exclude 5Ma and present-day to match Oscillayers data
  xyz_im1 <- temp_ppgem[, c(1,2,(i-1))]
  xyz_i <- temp_ppgem[, c(1,2,i)]
  delta_t_ppgem <- c(delta_t_ppgem, 
                     mean(abs(xyz_im1[, 3] - xyz_i[, 3]),
                          na.rm = T))
  # Rasterise corresponding tables
  r_im1 <- rasterFromXYZ(xyz_im1)
  r_i <- rasterFromXYZ(xyz_i)
  # North America
  im1_NA <- extract(r_im1, NthAm)[[1]]
  i_NA <- extract(r_i, NthAm)[[1]]
  delta_t_NA_ppgem <- c(delta_t_NA_ppgem,
                         mean(abs(im1_NA - i_NA), na.rm = T))
  
  # South America
  im1_SA <- extract(r_im1, SthAm)[[1]]
  i_SA <- extract(r_i, SthAm)[[1]]
  delta_t_SA_ppgem <- c(delta_t_SA_ppgem,
                         mean(abs(im1_SA - i_SA), na.rm = T))
  if(i %% 4 == 0){
    print(i)
  }
}

# Backwards-in-time
delta_t_ppgem <- rev(delta_t_ppgem)
delta_t_NA_ppgem <- rev(delta_t_NA_ppgem)
delta_t_SA_ppgem <- rev(rev(delta_t_SA_ppgem))

## Test for a difference -------------------------------------------------------
wilcox.test(delta_t_ppgem, delta_t_oscill, paired = T)

## Summarise and plot ----------------------------------------------------------
df <- data.frame(model = c(rep("PALEO-PGEM", length(delta_t_ppgem)),
                           rep("Oscillayers", length(delta_t_oscill))),
                 delta = c(delta_t_ppgem, delta_t_oscill),
                 Mean = c(rep(mean(delta_t_ppgem), length(delta_t_ppgem)),
                          rep(mean(delta_t_oscill), length(delta_t_oscill))),
                 time = rep(2:499, 2))
saveRDS(df, "./Data/Oscillayers/Delta_temp_comparison.RDS")

# Without pairwise lines
dev_plt <- df %>%
  ggplot(aes(x = model, y = delta)) +
  geom_violin(aes(fill = model), alpha = 0.5, linewidth = 0.1) +
  scale_fill_manual(values = c("#7fcdbb", "#fa9fb5")) +
  geom_point(size = 0.1, shape = 20) +
  labs(x = NULL, y = "Temperature deviation (°C)") +
  geom_point(aes(x = model, y = Mean), shape = 4, colour = "red", size = 2) +
  annotate(geom = "segment", x = 1, y = 9.5, yend = 10.035, linewidth = 0.7) +
  annotate(geom = "segment", x = 1, xend = 2, y = 10, linewidth = 0.7) +
  annotate(geom = "segment", x = 2, y = 8.4, yend = 10.035, linewidth = 0.7) +
  annotate(geom = "text", x = 1.5, y = 10.15, label = "***", size = 4, fontface = 2) +
  theme(panel.background = element_rect(fill = "grey95"),
        legend.position = "none",
        panel.grid = element_line(linewidth = 0.1),
        axis.line.y =  element_line(),
        axis.text = element_text(size = 5),
        axis.title.y = element_text(size = 8))

ggsave("./Figures/MS/Supp/Oscillayers/Deviation_plot.pdf", plot = dev_plt,
       height = 80, width = 100, units = "mm")

# With pairwise lines
dev_plt_pair <- dev_plt +
  geom_line(aes(group = time), linewidth = 0.1, alpha = 0.3)

ggsave("./Figures/MS/Supp/Oscillayers/Deviation_plot_pair.pdf", 
       plot = dev_plt_pair, height = 80, width = 100, units = "mm")
