library(tidyverse)

av_clim <- readRDS("./Data/PALEO_PGEM-bioclim/average_clim_both_continents.RDS")

av_clim |> 
  filter(continent == "North" & var == "Temperature") |> 
  ggplot(aes(x = time, y = clim_av)) + 
  geom_line() +
  scale_x_reverse() +
  theme(axis.line = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid = element_blank())

ggsave("./Figures/MS/Graphical_Abstract/temp_curve.pdf", height = 100, width = 200, units = "mm")
