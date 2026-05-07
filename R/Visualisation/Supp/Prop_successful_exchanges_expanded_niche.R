################################################################################
# Name: Prop_successful_exchanges_expanded_niche.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Plot proportion of successful exchanges across starting continents and
#       models, and compare when expanding thermal niche.
################################################################################

library(tidyverse)
library(ggpubr)

source("./R/useful/helper_functions.R")

################################################################################
############################ PPGEM-based landscape #############################
################################################################################

## Set up plotting dataset -----------------------------------------------------
plot_df_prop_success <- readRDS("./Results/Exchanged_metrics/Prop_successful_exchange.RDS")
plot_df_prop_success$w_max <- 0.1
plot_df_prop_success <- plot_df_prop_success %>% filter(Model %in% c("M0", "M1"))

crash <- readRDS("./Results/Exchanged_metrics/Crash_counter.RDS")
crash <- crash %>%
  select(Start, M0, M1) %>% 
  mutate(w_max = 0.1) %>% 
  filter(Start != "North America Standardised") %>% 
  group_by(Start, w_max) %>% 
  pivot_longer(cols = c(M0, M1), names_to = "model", values_to = "prop_crash") %>% 
  ungroup()

for(w in c(0.6, 1)){
  for(model in c("M0", "M1")){
    # Open summary param tables
    path <- list.files(paste0("./Data/Gen3sis_parameter_tables/Niche_expanded/w_", w,
                              "/", model),
                       pattern = "North_America_parameters_EXTENDED_EXCH", full.names = T)[[1]]
    param_tbl <- read.table(path, header = T)
    # store crash nb
    n_crash <- length(which(param_tbl$exchanged == -1))
    crash <- crash %>% add_row(Start = "North America", w_max = w,
                               model = model, prop_crash = n_crash/500)
    # Process
    param_tbl <- param_tbl %>% filter(exchanged != -1)
    n_success <- length(which(param_tbl$exchanged == 1))
    p_success <- n_success / nrow(param_tbl)
    lwr_ci <- bino_CI(prop = p_success, n = nrow(param_tbl), what = "Lower")
    upr_ci <- bino_CI(prop = p_success, n = nrow(param_tbl), what = "Upper")
    # Extend plotting df
    plot_df_prop_success <- plot_df_prop_success %>% 
      add_row(Ori = "North America", Nb_success = n_success, Prop_success = p_success, 
              Lower_CI = lwr_ci, Upper_CI = upr_ci, Model = model, w_max = w)
  }
}

plot_df_prop_success <- plot_df_prop_success %>% 
  mutate(cont_w = sapply(X = 1:nrow(plot_df_prop_success),
                         FUN = function(x){paste0(plot_df_prop_success$Ori[x], 
                                                  " \n(w_max=", plot_df_prop_success$w_max[x], ")")}))

suc_plot <- plot_df_prop_success %>% 
  ggplot(aes(x = cont_w, y = Prop_success)) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.15) +
  geom_point(aes(fill = Ori), colour = "black", pch = 23, size = 1.5) +
  scale_fill_manual(values = c("#fb6a4aff", "#66c2a4ff")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.12), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  facet_wrap(.~Model, ncol = 2) +
  labs(x = NULL, y = "Proportion of successful exchange") +
  theme(axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 5),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major = element_line(linewidth = 0.1, color = "grey50"),
        panel.grid.minor = element_line(linewidth = 0.1, color = "grey50"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        strip.background = element_rect(fill = "#DDE6F5"))

## Plot proportion of crashes
crash <- crash %>% mutate(cont_w = sapply(X = 1:nrow(crash),
                                          FUN = function(x){paste0(crash$Start[x], 
                                                                   " \n(w_max=", crash$w_max[x], ")")}))
crash_plot <- crash %>% 
  ggplot(aes(x = cont_w, y = prop_crash)) +
  geom_point(aes(fill = Start), colour = "black", pch = 21, size = 1.5) +
  scale_fill_manual(values = c("#fb6a4aff", "#66c2a4ff")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.65), breaks = c(0, 0.2, 0.4, 0.6)) +
  facet_wrap(.~model, ncol = 2) +
  labs(x = "Ancestral continent", y = "Proportion of crash") +
  theme(axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 5),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major = element_line(linewidth = 0.1, color = "grey50"),
        panel.grid.minor = element_line(linewidth = 0.1, color = "grey50"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        strip.background = element_rect(fill = "#DDE6F5"))

## Assemble and save
com_pnl <- ggarrange(suc_plot, crash_plot, nrow = 2, labels = c("(A)", "(B)"))
ggsave("./Figures/MS/Supp/niche_expanded/NA_vs_SA-PPGEM-diff_w.pdf", plot = com_pnl,
       height = 150, width = 130, units = "mm")
  
crash <- crash %>% 
  select(!cont_w)

saveRDS(crash, "./Results/Exchanged_metrics/Crash_counter_diff_w.RDS")


################################################################################
######################### Oscillayers-based landscape ##########################
################################################################################

## Set up plotting dataset -----------------------------------------------------
plot_df_prop_success_oscill <- readRDS("./Results/Exchanged_metrics/Oscillayers/Prop_successful_exchange.RDS")
plot_df_prop_success_oscill$w_max <- 0.1
plot_df_prop_success_oscill <- plot_df_prop_success_oscill %>% filter(Model %in% c("M0", "M1"))
plot_df_prop_success_oscill$Nb_success <- NA


crash_oscill <- data.frame(Start = NA, w_max = NA, model = NA, prop_crash = NA)
for(start in c("North_America", "South_America")){
  for(mdl in c("M0", "M1")){
    p_tbl <- read.table(paste0("./Data/Gen3sis_parameter_tables/Oscillayers/", mdl, "/", 
                        start, "_parameters_EXTENDED_EXCH_AREA_DIV_DIST.txt"),
                        header = T)
    p_crash <- length(which(p_tbl$exchanged == -1)) / 500
    crash_oscill <- crash_oscill %>% add_row(Start = paste0(strsplit(start, split = "_")[[1]][1],
                                                            " ", strsplit(start, split = "_")[[1]][2]),
                                             w_max = 0.1, 
                                             model = mdl, prop_crash = p_crash)
    # Extend Nb_success column from plot_df
    plot_df_prop_success_oscill$Nb_success[which(plot_df_prop_success_oscill$Model == mdl &
                                                   plot_df_prop_success_oscill$Ori == paste0(strsplit(start, split = "_")[[1]][1],
                                                   " ", strsplit(start, split = "_")[[1]][2]))] <- length(which(p_tbl$exchanged == 1))
  }
}
crash_oscill <- crash_oscill[2:nrow(crash_oscill),]


for(w in c(0.6, 1)){
  for(model in c("M0", "M1")){
    # Open summary param tables
    path <- list.files(paste0("./Data/Gen3sis_parameter_tables/Oscillayers_expanded/w_", w,
                              "/", model),
                       pattern = "North_America_parameters_EXTENDED_EXCH", full.names = T)[[1]]
    param_tbl <- read.table(path, header = T)
    # store crash nb
    n_crash <- length(which(param_tbl$exchanged == -1))
    crash_oscill <- crash_oscill %>% add_row(Start = "North America", w_max = w,
                               model = model, prop_crash = n_crash/500)
    # Process
    param_tbl <- param_tbl %>% filter(exchanged != -1)
    n_success <- length(which(param_tbl$exchanged == 1))
    p_success <- n_success / nrow(param_tbl)
    lwr_ci <- bino_CI(prop = p_success, n = nrow(param_tbl), what = "Lower")
    upr_ci <- bino_CI(prop = p_success, n = nrow(param_tbl), what = "Upper")
    # Extend plotting df
    plot_df_prop_success_oscill <- plot_df_prop_success_oscill %>% 
      add_row(Ori = "North America", Nb_success = n_success, Prop_success = p_success, 
              Lower_CI = lwr_ci, Upper_CI = upr_ci, Model = model, w_max = w)
  }
}

plot_df_prop_success_oscill <- plot_df_prop_success_oscill %>% 
  mutate(cont_w = sapply(X = 1:nrow(plot_df_prop_success_oscill),
                         FUN = function(x){paste0(plot_df_prop_success_oscill$Ori[x], 
                                                  " \n(w_max=", plot_df_prop_success_oscill$w_max[x], ")")}))

suc_plot_oscill <- plot_df_prop_success_oscill %>% 
  ggplot(aes(x = cont_w, y = Prop_success)) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.15) +
  geom_point(aes(fill = Ori), colour = "black", pch = 23, size = 1.5) +
  scale_fill_manual(values = c("#fb6a4aff", "#66c2a4ff")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.12), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  facet_wrap(.~Model, ncol = 2) +
  labs(x = NULL, y = "Proportion of successful exchange") +
  theme(axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 5),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major = element_line(linewidth = 0.1, color = "grey50"),
        panel.grid.minor = element_line(linewidth = 0.1, color = "grey50"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        strip.background = element_rect(fill = "#DDE6F5"))

## Plot proportion of crashes
crash_oscill <- crash_oscill %>% mutate(cont_w = sapply(X = 1:nrow(crash_oscill),
                                          FUN = function(x){paste0(crash_oscill$Start[x], 
                                                                   " \n(w_max=", crash_oscill$w_max[x], ")")}))
crash_plot_oscill <- crash_oscill %>% 
  ggplot(aes(x = cont_w, y = prop_crash)) +
  geom_point(aes(fill = Start), colour = "black", pch = 21, size = 1.5) +
  scale_fill_manual(values = c("#fb6a4aff", "#66c2a4ff")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.65), breaks = c(0, 0.2, 0.4, 0.6)) +
  facet_wrap(.~model, ncol = 2) +
  labs(x = "Ancestral continent", y = "Proportion of crash") +
  theme(axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 5),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major = element_line(linewidth = 0.1, color = "grey50"),
        panel.grid.minor = element_line(linewidth = 0.1, color = "grey50"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        strip.background = element_rect(fill = "#DDE6F5"))

## Assemble and save
com_pnl <- ggarrange(suc_plot_oscill, crash_plot_oscill, nrow = 2, labels = c("(A)", "(B)"))
ggsave("./Figures/MS/Supp/niche_expanded/NA_vs_SA-Oscillayers-diff_w.pdf", plot = com_pnl,
       height = 150, width = 130, units = "mm")

crash_oscill <- crash_oscill %>% 
  select(!cont_w)

saveRDS(crash_oscill, "./Results/Exchanged_metrics/Oscillayers/Crash_counter_diff_w_Oscillayers.RDS")