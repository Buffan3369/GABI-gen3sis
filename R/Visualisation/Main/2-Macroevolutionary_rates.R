################################################################################
# Name: 2-Macroevolutionary_rates.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Represent macroevolutionary rates through time
################################################################################

library(tidyverse)
library(ggpubr)
library(deeptime)

## M1 equal area ---------------------------------------------------------------
# North
div_rates_North <- readRDS("./Results/M1_eq_area/North_America_start/North_America_div_rates_bin_100.RDS")
div_rates_North$Time <- div_rates_North$Time/100 # convert time to Ma
  # Mean rate per bin
mean_sp_rate <- lapply(X = 1:5, 
                      FUN = function(x){
                        sp_bin <- div_rates_North$sp_rate[which(div_rates_North$Bin == x)]
                        return(rep(mean(sp_bin, na.rm = T), 100))
                      })
mean_ex_rate <- lapply(X = 1:5, 
                       FUN = function(x){
                         ex_bin <- div_rates_North$ex_rate[which(div_rates_North$Bin == x)]
                         return(rep(mean(ex_bin, na.rm = T), 100))
                       })
L <- nrow(div_rates_North) / 500
div_rates_North$mean_sp <- rep(unlist(mean_sp_rate), L)
div_rates_North$mean_ex <- rep(unlist(mean_ex_rate), L)

sp_north_plot <- div_rates_North %>% 
  ggplot(aes(x = Time, y = sp_rate, group = Run)) +
  scale_x_reverse() +
  geom_step(colour = "#9ecae1", linewidth = 0.25) +
  geom_step(aes(y = mean_sp), colour = "#08519c", linewidth = 1) +
  labs(x = "Time (Ma)", y = "Simulated rate (event/Myr)", title = "Speciation") +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 15, hjust = 0.5, colour = "#08519c"),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5)) +
  coord_geo(dat = "epochs", height = unit(1, "line"))

ex_north_plot <- div_rates_North %>% 
  ggplot(aes(x = Time, y = ex_rate, group = Run)) +
  scale_x_reverse() +
  scale_y_continuous(limits = c(0, 3)) +
  geom_step(colour = "#fc9272", linewidth = 0.25) +
  geom_step(aes(y = mean_ex), colour = "#a50f15", linewidth = 1) +
  labs(x = "Time (Ma)", y = NULL, title = "Extinction") +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 15, hjust = 0.5, colour = "#a50f15"),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5)) +
  coord_geo(dat = "epochs", height = unit(1, "line"))

north_div_rates <- ggarrange(sp_north_plot, ex_north_plot, ncol = 2)
ggsave("./Figures/div_rates/North_start_diversification_rates_M1_eq_area.pdf", north_div_rates, 
       height = 100, width = 200, units = "mm")

rm(list = ls())


# South
div_rates_South <- readRDS("./Results/M1_eq_area/South_America_start/South_America_div_rates_bin_100.RDS")
div_rates_South$Time <- div_rates_South$Time/100 # convert time to Ma
# Mean rate per bin
mean_sp_rate <- lapply(X = 1:5, 
                       FUN = function(x){
                         sp_bin <- div_rates_South$sp_rate[which(div_rates_South$Bin == x)]
                         return(rep(mean(sp_bin, na.rm = T), 100))
                       })
mean_ex_rate <- lapply(X = 1:5, 
                       FUN = function(x){
                         ex_bin <- div_rates_South$ex_rate[which(div_rates_South$Bin == x)]
                         return(rep(mean(ex_bin, na.rm = T), 100))
                       })
L <- nrow(div_rates_South) / 500
div_rates_South$mean_sp <- rep(unlist(mean_sp_rate), L)
div_rates_South$mean_ex <- rep(unlist(mean_ex_rate), L)

sp_south_plot <- div_rates_South %>% 
  ggplot(aes(x = Time, y = sp_rate, group = Run)) +
  scale_x_reverse() +
  geom_step(colour = "#9ecae1", linewidth = 0.25) +
  geom_step(aes(y = mean_sp), colour = "#08519c", linewidth = 1) +
  labs(x = "Time (Ma)", y = "Simulated rate (event/Myr)", title = "Speciation") +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 15, hjust = 0.5, colour = "#08519c"),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5)) +
  coord_geo(dat = "epochs", height = unit(1, "line"))

ex_south_plot <- div_rates_South %>% 
  ggplot(aes(x = Time, y = ex_rate, group = Run)) +
  scale_x_reverse() +
#  scale_y_continuous(limits = c(0, 3)) +
  geom_step(colour = "#fc9272", linewidth = 0.25) +
  geom_step(aes(y = mean_ex), colour = "#a50f15", linewidth = 1) +
  labs(x = "Time (Ma)", y = NULL, title = "Extinction") +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 15, hjust = 0.5, colour = "#a50f15"),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5)) +
  coord_geo(dat = "epochs", height = unit(1, "line"))

south_div_rates <- ggarrange(sp_south_plot, ex_south_plot, ncol = 2)
ggsave("./Figures/div_rates/South_start_diversification_rates_M1_eq_area.pdf", south_div_rates, 
       height = 100, width = 200, units = "mm")
