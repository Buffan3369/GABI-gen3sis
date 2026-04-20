################################################################################
# Name: Prop_successful_exchanges_expanded_niche.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Plot proportion of successful exchanges across starting continents and
#       models, and compare when expanding thermal niche.
################################################################################

library(tidyverse)

source("./R/useful/helper_functions.R")

## Set up plotting dataset -----------------------------------------------------
plot_df_prop_success <- readRDS("./Results/Exchanged_metrics/Prop_successful_exchange.RDS")
plot_df_prop_success$w_max <- 0.1

for(w in c(0.6, 1)){
  for(model in c("M0", "M1")){
    # Open summary param tables
    path <- list.files(paste0("./Data/Gen3sis_parameter_tables/Niche_expanded/w_", w,
                              "/", model),
                       pattern = "North_America_parameters_EXTENDED_EXCH", full.names = T)[[1]]
    param_tbl <- read.table(path, header = T)
    # Process
    param_tbl <- param_tbl %>% filter(exchanged != -1)
    p_success <- length(which(param_tbl$exchanged == 1)) / nrow(param_tbl)
    lwr_ci <- bino_CI(prop = p_success, n = nrow(param_tbl), what = "Lower")
    upr_ci <- bino_CI(prop = p_success, n = nrow(param_tbl), what = "Upper")
    # Extend plotting df
    plot_df_prop_success <- plot_df_prop_success %>% 
      add_row(Ori = "North America", Prop_success = p_success, Lower_CI = lwr_ci,
              Upper_CI = upr_ci, Model = model, w_max = w)
  }
}

ptbl_NA_0.6_M0 <- read.table("./Data/Gen3sis_parameter_tables/Niche_expanded/w_0.6/M0/North_America_parameters_EXTENDED_EXCH_AREA_DIV_DIST.txt",
                             header = T)
ptbl_NA_0.6_M1 <- read.table("./Data/Gen3sis_parameter_tables/Niche_expanded/w_0.6/M1/North_America_parameters_EXTENDED_EXCH_AREA_DIV_DIST.txt",
                             header = T)

# w in [0.01, 1]


prop_plot <- plot_df_prop_success %>% 
  ggplot(aes(x = Ori, y = Prop_success)) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.15) +
  geom_point(aes(fill = Ori), colour = "black", pch = 23, size = 1.5) +
  scale_fill_manual(values = c("#fb6a4aff", "#66c2a4ff")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.12), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  labs(x = NULL, y = NULL) +
  facet_wrap(.~Model, ncol = 2) +
  labs(x = "Ancestral continent", y = "Proportion of successful exchange") +
  theme(axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 5),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major = element_line(linewidth = 0.1, color = "grey50"),
        panel.grid.minor = element_line(linewidth = 0.1, color = "grey50"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        strip.background = element_rect(fill = "#DDE6F5"))

ggsave("./Figures/MS/Main/Figure_PropSuccess/Figure_PropSuccess.png", 
       plot = prop_plot, height = 120, width = 110, dpi = 600, units = "mm")

## Incorporating simulations that crashed --------------------------------------
plot_df_prop_success_raw <- readRDS("./Results/Exchanged_metrics/RawProp_successful_exchange.RDS")

prop_plot2 <- plot_df_prop_success_raw %>% 
  filter(Model %in% c("M0", "M1")) %>% 
  ggplot(aes(x = Ori, y = Prop_success)) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.15) +
  geom_point(aes(fill = Ori), colour = "black", pch = 23, size = 1.5) +
  scale_fill_manual(values = c("#fb6a4aff", "#66c2a4ff")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.12), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  labs(x = NULL, y = NULL) +
  facet_wrap(.~Model, ncol = 2) +
  labs(x = "Ancestral continent", y = "Proportion of successful exchange") +
  theme(axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 5),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major = element_line(linewidth = 0.1, color = "grey50"),
        panel.grid.minor = element_line(linewidth = 0.1, color = "grey50"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        strip.background = element_rect(fill = "#DDE6F5"))

ggsave("./Figures/MS/Supp/Figure_RawPropSuccessM0M1.png", 
       plot = prop_plot2, height = 75, width = 120, dpi = 600, units = "mm")
