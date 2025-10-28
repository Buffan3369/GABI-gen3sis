################################################################################
# Name: ColTime_densities.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Plot densities of colonisation times.
################################################################################

library(tidyverse)


for(mdl in c("M0", "M1", "M2", "M3")){
  for(start in c("North", "South")){
    tbl <- readRDS(paste0("./Data/Gen3sis_parameter_tables/", mdl, "/", 
                          start, "_America_parameters_EXTENDED_EXCH_AREA_DIV_DIST_biome_time.RDS"))
    tbl <- tbl %>% filter(time_exch != -1)
    dens <- density(tbl$time_exch)
    if(mdl == "M0" & start == "North"){
      plot_df <- data.frame(x = dens$x,
                            y = dens$y,
                            model = mdl,
                            start = start)
    }
    else{
      plot_df <- rbind.data.frame(plot_df,
                                  data.frame(x = dens$x,
                                             y = dens$y,
                                             model = mdl,
                                             start = start))
    }
  }
}

plot_df %>%
  filter(x < 500 & x > 0) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  facet_grid(model~start)
