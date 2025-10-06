################################################################################
# Name: 3-Area_Div_Dist_panels.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Display postprocessing metrics
################################################################################

library(tidyverse)
source("./R/useful/helper_functions.R")

################################################################################
############################## 0. Preprocessing ################################
################################################################################

## Loop across models to create plot datasets -----------------------------------
for(mdl in c("M0", "M1", "M2", "M3")){
  NA_recap_tbl <- read.table(paste0("./Data/Gen3sis_parameter_tables/", mdl, "/North_America_parameters_EXTENDED_EXCH_AREA_DIV_DIST_biome.txt"),
                             header = T, sep = "\t")
  SA_recap_tbl <- read.table(paste0("./Data/Gen3sis_parameter_tables/", mdl, "/South_America_parameters_EXTENDED_EXCH_AREA_DIV_DIST_biome.txt"),
                             header = T, sep = "\t")
  
  ## Filter out simulations that crashed ---------------------------------------
  cat("Number of simulations that crashed for ", mdl, " with North American ancestor: ", length(which(NA_recap_tbl$exchanged == -1)), "\n")
  NA_recap_tbl <- NA_recap_tbl %>% filter(!(exchanged == -1))
  cat("Number of simulations that crashed for ", mdl, " with South American ancestor: ", length(which(SA_recap_tbl$exchanged == -1)), "\n")
  SA_recap_tbl <- SA_recap_tbl %>% filter(!(exchanged == -1))
  
  ## Success tables (= simulations resulting in a successful exchange) ---------
  NA_success_tbl <- NA_recap_tbl %>% filter(exchanged == 1)
  SA_success_tbl <- SA_recap_tbl %>% filter(exchanged == 1)
  
  ## 1. Success proportions ----------------------------------------------------
  na_success <- sum(NA_recap_tbl$exchanged) / nrow(NA_recap_tbl)
  sa_success <- sum(SA_recap_tbl$exchanged) / nrow(SA_recap_tbl)
  
  ## 2. Proportion of colonised area -------------------------------------------
  na_prop_col_area <- mean(NA_success_tbl$prop_col_area)

  ## 3. Diversity in colonised area --------------------------------------------
  na_mean_div <- mean(NA_success_tbl$div_col)
  na_sd_div <- sd(NA_success_tbl$div_col)
  sa_mean_div <- mean(SA_success_tbl$div_col)
  sa_sd_div <- sd(SA_success_tbl$div_col)

  ## 4. 
  
  if(mdl == "M0"){
    # Proportion of success (associated CI from binomial)
    plot_df_prop_success <- data.frame(Ori = c("North America", "South America"),
                                       Prop_success = c(na_success, sa_success),
                                       Lower_CI = c(bino_CI(prop = na_success,
                                                            n = nrow(NA_recap_tbl),
                                                            alpha = 0.05,
                                                            what = "Lower"),
                                                    bino_CI(prop = sa_success,
                                                            n = nrow(SA_recap_tbl),
                                                            alpha = 0.05,
                                                            what = "Lower")),
                                       Upper_CI = c(bino_CI(prop = na_success,
                                                            n = nrow(NA_recap_tbl),
                                                            alpha = 0.05,
                                                            what = "Upper"),
                                                    bino_CI(prop = sa_success,
                                                            n = nrow(SA_recap_tbl),
                                                            alpha = 0.05,
                                                            what = "Upper")),
                                       Model = c(mdl, mdl))
  }
  else{
    # Proportion of success (associated CI from binomial)
    plot_df_prop_success <- plot_df_prop_success %>% 
      add_row(Ori = c("North America", "South America"),
              Prop_success = c(na_success, sa_success),
              Lower_CI = c(bino_CI(prop = na_success,
                                   n = nrow(NA_recap_tbl),
                                   alpha = 0.05,
                                   what = "Lower"),
                           bino_CI(prop = sa_success,
                                   n = nrow(SA_recap_tbl),
                                   alpha = 0.05,
                                   what = "Lower")),
              Upper_CI = c(bino_CI(prop = na_success,
                                   n = nrow(NA_recap_tbl),
                                   alpha = 0.05,
                                   what = "Upper"),
                           bino_CI(prop = sa_success,
                                   n = nrow(SA_recap_tbl),
                                   alpha = 0.05,
                                   what = "Upper")),
              Model = c(mdl, mdl))
  }
}


################################################################################
#################### 1. PROPORTION OF SUCCESSFUL EXCHANGES #####################
################################################################################

prop_plot2 <- plot_df_prop_success %>% 
  ggplot(aes(x = Ori, y = Prop_success)) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.15) +
  geom_point(aes(colour = Ori), size = 1.5) +
  scale_colour_manual(values = c("#fb6a4aff", "#66c2a4ff")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.12), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  labs(x = NULL, y = NULL) +
  facet_grid(.~Model) +
  labs(x = "Ancestral area", y = "Prop. successful exchange") +
  theme(axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 5),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major = element_line(linewidth = 0.1, color = "grey50"),
        panel.grid.minor = element_line(linewidth = 0.1, color = "grey50"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        strip.background = element_rect(fill = "#DDE6F5"))

ggsave("./Figures/prop_successful_exch/Panel_point_CI.pdf", 
       plot = prop_plot2, height = 80, width = 170, units = "mm")

ggsave("./Figures/prop_successful_exch/Panel_point_CI.png", 
       plot = prop_plot2, height = 80, width = 170, dpi = 600, units = "mm")


################################################################################
##################### 2. DIVERSITY IN THE COLONISED AREA #######################
################################################################################

