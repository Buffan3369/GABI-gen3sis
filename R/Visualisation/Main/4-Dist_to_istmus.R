################################################################################
# Name: 4-Dist_to_istmus.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Violin plots for distance between simulation starting point and isthmus
################################################################################

library(tidyverse)

## Assemble large dataset ------------------------------------------------------
P_tbl <- read.table(paste0("./Data/Gen3sis_parameter_tables/M0/North_America_parameters_EXTENDED_EXCH_AREA_DIV_DIST.txt"),
                    header = T)
P_tbl <- P_tbl %>% 
  # remove aberrant distances
  filter(dist_to_isthmus <= 80000) %>%
  # convert to km
  mutate(exchanged = as.factor(exchanged),
         dist_to_isthmus = sapply(dist_to_isthmus, function(x){x/10}),
         model = "M0",
         start_region = "North")

for(mdl in c("M0", "M1", "M2", "M3")){
  for(start in c("North", "South")){
    if(mdl == "M0" & start == "North"){
      next
    }
    param_tbl <- read.table(paste0("./Data/Gen3sis_parameter_tables/", mdl,
                                   "/", start, "_America_parameters_EXTENDED_EXCH_AREA_DIV_DIST.txt"),
                            header = T)
    
    param_tbl <- param_tbl %>% 
      # remove aberrant distances
      filter(dist_to_isthmus <= 80000) %>%
      # convert to km
      mutate(exchanged = as.factor(exchanged),
             dist_to_isthmus = sapply(dist_to_isthmus, function(x){x/10}),
             model = mdl,
             start_region = start)
    # Adjust in case one of the two climatic breadths are missing (M2 or M3)
    if(mdl == "M2"){
      param_tbl <- param_tbl %>% add_column(omega_P = NA, .after = "omega_T")
    }
    if(mdl == "M3"){
      param_tbl <- param_tbl %>% add_column(omega_T = NA, .before = "omega_P")
    }
    # Merge
    P_tbl <- rbind.data.frame(P_tbl, param_tbl)
    }
}

## Plot with simulations that crashed ------------------------------------------
plot_crash <- P_tbl %>% 
  ggplot(aes(x = exchanged, y = dist_to_isthmus)) +
  geom_violin(adjust = .75, draw_quantiles = c(0.5), scale = "width", aes(fill = exchanged)) +
  scale_fill_manual(values = c("grey80", "#f7fcb9", "#78c679")) +
  geom_point(size = 0.05) +
  labs(x = "Exchange success", y = "Distance to isthmus (km)") +
  facet_grid(start_region~model) +
  theme(legend.position = "none",
        axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 6),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        panel.background = element_blank(),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

ggsave(paste0("./Figures/dist_to_isthm/distance_violin_panel_CRASH.pdf"), 
       plot = plot_crash, height = 120, width = 200, units = "mm")

## Plot without simulations that crashed ---------------------------------------
plot_NoCrash <- P_tbl %>% 
  filter(exchanged != -1) %>% 
  ggplot(aes(x = exchanged, y = dist_to_isthmus)) +
  geom_violin(adjust = .75, draw_quantiles = c(0.5), scale = "width", aes(fill = exchanged)) +
  scale_fill_manual(values = c( "#f7fcb9", "#78c679")) +
  geom_point(size = 0.05) +
  labs(x = "Exchange success", y = "Distance to isthmus (km)") +
  facet_grid(start_region~model) +
  theme(legend.position = "none",
        axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 6),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        panel.background = element_blank(),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

ggsave(paste0("./Figures/dist_to_isthm/distance_violin_panel_NoCRASH.pdf"), 
       plot = plot_NoCrash, height = 120, width = 170, units = "mm")
