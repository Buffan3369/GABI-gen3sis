################################################################################
# Name: 3b-All_starting_range_violin.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Violin plots for initial range of all simulation ancestors
################################################################################

library(tidyverse)
source("./R/useful/helper_functions.R")

################################################################################
################################# DATA PICKING #################################
################################################################################

param_tbl <- data.frame(dist_to_isthmus = NA,
                        model = NA,
                        start = NA,
                        stand = NA,
                        mean_dist = NA,
                        lower_ci = NA,
                        upper_ci = NA)

for(mdl in c("M0", "M1")){
  for(start in c("North","South")){
    if(start == "North"){
      ## Standardised North
      p_tbl <- read.table(paste0("./Data/Gen3sis_parameter_tables/Equal_dist/",
                                 mdl, "/", start, "_America_parameters_EqDist_EXTENDED_EXCH_AREA_DIV_DIST_biome.txt"),
                          sep = "\t", header = TRUE)
      p_tbl$model <- mdl
      p_tbl$start <- start
      p_tbl$stand <- "Standardised"
      p_tbl <- p_tbl %>% 
        select(dist_to_isthmus, model, start, stand)
      
      MeanDist <- mean(p_tbl$dist_to_isthmus, na.rm = T)
      p_tbl$mean_dist <- MeanDist
      p_tbl$lower_ci <- Student_CI(x_bar = MeanDist,
                                   n = 500,
                                   sigma = sd(p_tbl$dist_to_isthmus, na.rm = T),
                                   what = "Lower")
      p_tbl$upper_ci <- Student_CI(x_bar = MeanDist,
                                   n = 500,
                                   sigma = sd(p_tbl$dist_to_isthmus, na.rm = T),
                                   what = "Upper")
  
      param_tbl <- rbind.data.frame(param_tbl, p_tbl)
      
      rm(p_tbl)
      ## Double South
      p_tbl <- read.table(paste0("./Data/Gen3sis_parameter_tables/", mdl, 
                                 "/South_America_parameters_EXTENDED_EXCH_AREA_DIV_DIST_biome.txt"),
                          sep = "\t", header = TRUE)
      p_tbl$model <- mdl
      p_tbl$start <- "South"
      p_tbl$stand <- "Standardised"
      p_tbl <- p_tbl %>% 
        select(dist_to_isthmus, model, start, stand)
      
      MeanDist <- mean(p_tbl$dist_to_isthmus, na.rm = T)
      p_tbl$mean_dist <- MeanDist
      p_tbl$lower_ci <- Student_CI(x_bar = MeanDist,
                                   n = 500,
                                   sigma = sd(p_tbl$dist_to_isthmus, na.rm = T),
                                   what = "Lower")
      p_tbl$upper_ci <- Student_CI(x_bar = MeanDist,
                                   n = 500,
                                   sigma = sd(p_tbl$dist_to_isthmus, na.rm = T),
                                   what = "Upper")
      
      param_tbl <- rbind.data.frame(param_tbl, p_tbl)
    }
    # Unstandardised
    rm(p_tbl)
    p_tbl <- read.table(paste0("./Data/Gen3sis_parameter_tables/", mdl, "/", start,
                               "_America_parameters_EXTENDED_EXCH_AREA_DIV_DIST_biome.txt"),
                        sep = "\t", header = TRUE)
    p_tbl$model <- mdl
    p_tbl$start <- start
    p_tbl$stand <- "Unstandardised"
    p_tbl <- p_tbl %>% 
      select(dist_to_isthmus, model, start, stand)
    
    MeanDist <- mean(p_tbl$dist_to_isthmus, na.rm = T)
    p_tbl$mean_dist <- MeanDist
    p_tbl$lower_ci <- Student_CI(x_bar = MeanDist,
                                 n = 500,
                                 sigma = sd(p_tbl$dist_to_isthmus, na.rm = T),
                                 what = "Lower")
    p_tbl$upper_ci <- Student_CI(x_bar = MeanDist,
                                 n = 500,
                                 sigma = sd(p_tbl$dist_to_isthmus, na.rm = T),
                                 what = "Upper")
    
    param_tbl <- rbind.data.frame(param_tbl, p_tbl)
  }
}
param_tbl <- param_tbl[2:nrow(param_tbl),]
rownames(param_tbl) <- 1:nrow(param_tbl)


## All simulations -------------------------------------------------------------
param_tbl %>% 
  ggplot(aes(x = start, y = dist_to_isthmus)) +
  geom_violin(adjust = .75, scale = "width", linewidth = 0.1, aes(fill = factor(start))) +
  geom_point(aes(y = mean_dist, colour = factor(start)), size = 2) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.08) +
  scale_fill_manual(values = c("#fcbba1", "#ccece6")) +
  scale_colour_manual(values = c("#fb6a4aff", "#66c2a4ff")) +
  labs(x = "Ancestral region", y = "Distance to isthmus (km)") +
  theme(legend.position = "none",
        axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 6),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        strip.background = element_rect(fill = "#DDE6F5"),
        panel.background = element_rect(fill = "#F5F0FF"),
        panel.grid.major = element_line(linewidth = 0.25),
        panel.grid.minor = element_line(linewidth = 0.25),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")
  ) +
  facet_grid(stand~model)

ggsave(paste0("./Figures/dist_to_isthm/All_starting_ranges.pdf"), 
       plot = plot_all_starting_range, height = 120, width = 200, units = "mm")

ggsave(paste0("./Figures/dist_to_isthm/All_starting_ranges.png"), 
       plot = plot_all_starting_range, height = 120, width = 200, dpi = 600, units = "mm")



param_tbl %>% 
  ggplot(aes(x = start, y = dist_to_isthmus)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_point() +
  facet_grid(.~model)
